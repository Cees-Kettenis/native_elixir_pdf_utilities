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
application-wide memory cap or timeout. Render budgets cover one synchronous
operation, including repeated header/footer templates. Concurrent requests each
have their own budget; applications should also limit concurrency.

## PDF inputs

These limits apply when reading existing PDFs.

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_numeric_magnitude` | 1,000,000,000 | Absolute numeric operands and font metrics used for text extraction, and shared PDF geometry arrays |
| `max_pdf_input_bytes` | 50,000,000 | One PDF input |
| `max_pdf_reader_decoded_bytes` | 50,000,000 | Decoded bytes across object streams, xref streams, revisions, candidate attempts, and each intermediate filter stage in one reader call |
| `max_pdf_reader_tokens` | 1,000,000 | Tokens produced across all parsing passes in one reader call |
| `max_pdf_reader_values` | 500,000 | Parsed values and expanded xref entries across one reader call |
| `max_pdf_reader_work` | 250,000,000 | Bytes scanned by tokenizers, xref searches, stream decoding and predictor processing, including repeated passes |
| `max_pdf_container_entries` | 100,000 | Entries in one array or dictionary, including repeated dictionary keys |
| `max_pdf_numeric_token_bytes` | 1,024 | Decimal numeric-token bytes before integer/float conversion, and bytes in one binary xref integer |
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

Reader budgets span the complete synchronous operation, including nested probes
and candidate recovery. A failed candidate does not refund its work. Exhaustion
returns a diagnostic and releases the budget before another call starts. Stream
decoding requested separately through `Reader.decoded_stream/2` gets a fresh
budget; callers repeatedly decoding content still need their operation's own
aggregate limit, as text extraction already has. Flate output is drained in
bounded chunks, and expanding filters stop at the remaining decoded allowance.
These counters bound parsing work and retained structures, not total BEAM memory
or elapsed time. Configure a caller-owned timeout and concurrency ceiling too.

## HTML processing and output

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_html_source_bytes` | 2,000,000 | One HTML source or advanced tree text node |
| `max_aggregate_html_source_bytes` | 10,000,000 | Body and all expanded furniture sources |
| `max_css_source_bytes` | 1,000,000 | One stylesheet or inline declaration source |
| `max_aggregate_css_source_bytes` | 20,000,000 | CSS bytes processed, including repeated parsing passes and furniture |
| `max_html_nodes` | 25,000 | HTML token processing steps across a render, and nodes in an advanced input tree |
| `max_html_depth` | 128 | Nested HTML elements or advanced input tree depth |
| `max_css_rules` | 10,000 | Parsed stylesheet rules across a render |
| `max_css_work` | 5,000,000 | CSS scanned bytes, selector parsing work, and selector comparisons |
| `max_layout_boxes` | 100,000 | Drawing-box construction attempts, including measurements and furniture |
| `max_rendered_text_bytes` | 10,000,000 | Text production bytes, including generated-content parts before joining and transformations |
| `max_layout_text_work` | 20,000,000 | Text bytes measured and glyph-width entries visited, including repeated measurements |
| `max_table_grid_work` | 1,000,000 | Table row scans, column expansion and probes, and collapsed-border edge visits, reserved before expanding spans |
| `max_rendered_pages` | 1,000 | Generated PDF pages, also bounded by `max_pdf_pages` |
| `max_rendered_pdf_bytes` | 50,000,000 | Serialized PDF output before flattening its iodata |

Work counters charge repeated processing rather than just retained results.
Reaching a limit aborts the operation with an actionable diagnostic and clears
its budget. Standalone parser, style, layout, pagination, furniture, and writer
calls create their own budget. Nested pipeline stages share the active budget.
CSS source validation retains its `:invalid_css` diagnostic contract; other new
render limits return `:resource_limit_exceeded`.

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
| `max_css_variable_bytes` | 1,000,000 | One CSS value before and after variable substitution |
| `max_css_variable_total_bytes` | 10,000,000 | Aggregate computed CSS value bytes across one render |
| `max_css_variable_work` | 100,000 | Custom-property resolutions, including cached and repeated references, across one render |
| `max_css_variable_depth` | 64 | Custom-property dependency depth |
| `max_css_numeric_magnitude` | 1,000,000,000 | Absolute CSS numeric operands and converted lengths, excluding identifiers and quoted text |
| `max_layout_cardinality` | 1,000 | Grid tracks, placements, and table spans |

CSS budgets include unused custom properties. Dependencies are memoized during
each custom-property computation, and output sizes are checked before building
expanded binaries. Cycles and missing variables retain their existing invalid-value
behavior. A limit failure returns `:resource_limit_exceeded` at the `:limits` stage.

See [render options and supported formats](html-to-pdf-compatibility.md).

## Fonts

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_font_kerning_pairs` | 100,000 | Kerning subtables and glyph pairs inspected per font |
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

## Forms and attachments

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_form_fields` | 10,000 | HTML controls and visited field/widget nodes |
| `max_pdf_form_depth` | 64 | AcroForm field-tree nesting |
| `max_pdf_form_text_bytes` | 1,000,000 | Field names and filled text values |
| `max_pdf_attachments` | 1,000 | Total embedded-file count |
| `max_pdf_attachment_bytes` | 10,000,000 | One new attachment's bytes |
| `max_pdf_attachment_total_bytes` | 25,000,000 | New payload bytes plus existing stored attachment stream bytes |
| `max_mime_container_bytes` | 10,000,000 | ZIP bytes inspected for MIME evidence |
| `max_mime_container_entries` | 10,000 | ZIP central-directory entries inspected |

Form appearances also use `max_pdf_objects` and the existing font limits.
Attachment metadata uses `max_pdf_info_value_bytes`; attachment name-tree
traversal uses `max_pdf_name_tree_nodes` and `max_pdf_value_depth`. Existing
attachment streams are counted as stored, without decompression. Declared
uncompressed sizes are metadata, not a decompressed-byte budget. Document
reading also enforces the existing PDF input limits. Incremental updates retain
the original binary, which counts toward the output byte limit described below.

## Appearance expansion and incremental output

`max_appearance_text_bytes` defaults to 5,000,000 bytes across form and stamp
appearances. Repeating text on multiple pages or widgets charges each copy.
`max_appearance_widgets` defaults to 10,000 generated appearances or selected
widgets; HTML controls reserve both their normal and alternate appearances.
These limits are checked before appearance rendering.

The shared incremental writer checks serialized pieces as they accumulate. All
incremental writers also check the complete iodata before
flattening it. Output must fit both `max_pdf_input_bytes` and
`max_rendered_pdf_bytes`, so an edit cannot return a document already too large
for the reader under the current byte limit. Other reader limits still apply.

## Font sources and caches

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_font_source_bytes` | 10,000,000 | One configured, document-selected, bundled, or discovered font |
| `max_aggregate_font_source_bytes` | 40,000,000 | Distinct font byte payloads used during one render, including furniture |
| `max_font_count` | 128 | Family/weight/style combinations, including bundled fallbacks |
| `max_font_candidates` | 256 | Font source lookup and parse attempts; repeated file snapshots reuse the render-local result |
| `max_font_discoveries` | 64 | Distinct system discovery requests in one render, including process-cache hits |
| `max_font_cache_bytes` | 100,000,000 | Retained font-file cache entries |
| `max_system_font_cache_bytes` | 100,000,000 | Retained system-discovery cache entries |

File reads bind the regular-file and size checks to the opened handle and read
at most the available byte allowance plus one sentinel byte. Actual returned
bytes are checked again. Caller-configured paths are trusted. Document-selected local assets also undergo the path checks described below.

Configured bytes and document asset callbacks undergo the same font budgets.
Repeated sources and parsed payloads are reused within a render. Process-wide
caches evict entries by both count and retained size, conservatively including
heap structures and binary payloads. Oversized cache entries can still serve a
bounded request but are not retained. Resource failures are not cached.

System discovery reads trusted installed fonts through its native dependency,
which returns complete bytes rather than a file handle or path. Request count
and returned bytes are bounded, but this API cannot enforce the source-byte
limit before that native allocation. Applications that need to control every
font read should set `system_font_discovery: false` and supply approved font
bytes or files. Resolver callbacks likewise control their own I/O before
returning bytes. None of these per-render settings imposes a global concurrency
or native-memory cap.

## File input and local assets

HTML source files, configured CSS files, and PDF text-extraction files use
bounded regular-file reads before parsing. Their ceilings are
`max_html_source_bytes`, `max_css_source_bytes`, and `max_pdf_input_bytes`.
Image and font asset reads use the smaller of the individual source allowance
and the remaining aggregate allowance. Metadata and reads use the same opened
handle, and reads stop at the allowance plus one sentinel byte even if a file
grows after its metadata is checked.

Local asset reads use the same portable reader on Windows, macOS, and Linux.
No Python installation is required. `:base_url` rejects traversal and existing
symlinks beneath the base directory before opening a file. These pathname
checks cannot prevent concurrent filesystem changes. Keep file paths and asset
directories under trusted control, or supply approved bytes through `:assets`
or `:asset_resolver`. Byte limits do not impose an I/O timeout.
