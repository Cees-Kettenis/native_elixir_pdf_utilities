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
    max_operation_timeout_ms: 300_000,
    max_operation_memory_bytes: 1_073_741_824,
    max_merge_inputs: 20
  ]
```

Unspecified settings keep their defaults. Restart the application after a
change. Limits apply across the application and cannot be overridden for one
call. Invalid settings prevent startup and identify the problem.

Values must be positive integers. Aggregate image and metadata limits must
allow at least one maximum-size item.

## Operation time and memory

Document operations and PDF reads run in disposable workers. Nested calls share
one allowance. Applications control concurrency.

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_operation_timeout_ms` | 300,000 | Five-minute operation deadline |
| `max_operation_memory_bytes` | 1,073,741,824 | Observed worker memory and referenced binaries, with a GC heap cap |
| `operation_memory_check_interval_ms` | 25 | Milliseconds between memory checks |

Observed excess returns `:resource_limit_exceeded`. Unexpected worker exits,
including GC heap kills, return `:operation_worker_failed`.

Memory sampling can miss peaks and excludes native allocations, caches and
helper processes. Native work may continue after a timeout. Use OS process or
container isolation for hard memory and cancellation guarantees.

Asset resolvers run in the worker and share its allowance. See
[resolver process context](html-to-pdf-compatibility.md#resolver-process-context)
for migration guidance on caller-local data and messages.

## PDF inputs

These limits apply when reading existing PDFs.

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_numeric_magnitude` | 1,000,000,000 | Absolute numeric operands and font metrics used for text extraction, shared PDF geometry arrays, and output bookmark coordinates |
| `max_pdf_input_bytes` | 50,000,000 | One PDF input or generated output |
| `max_pdf_reader_decoded_bytes` | 50,000,000 | Decoded bytes across object streams, xref streams, revisions, candidate attempts, and each intermediate filter stage in one reader call |
| `max_pdf_reader_tokens` | 1,000,000 | Tokens produced across all parsing passes in one reader call |
| `max_pdf_reader_values` | 500,000 | Parsed values and expanded xref entries across one reader call |
| `max_pdf_reader_work` | 250,000,000 | Bytes scanned by tokenizers, xref searches and stream decoding, plus predictor sample/group processing and every LZW code, including resets and end codes |
| `max_pdf_predictor_working_bytes` | 160,000,000 | Conservative packed-buffer allowance for one TIFF/PNG predictor reconstruction, checked before buffers are allocated |
| `max_pdf_container_entries` | 100,000 | Entries in one parsed or generated PDF array or dictionary, including text-content containers and repeated dictionary keys |
| `max_pdf_numeric_token_bytes` | 1,024 | Decimal numeric-token bytes before integer/float conversion, output bookmark numbers, and bytes in one binary xref integer |
| `max_pdf_objects` | 100,000 | Xref entries including free object zero, and the maximum allocated object ID |
| `max_pdf_object_stream_entries` | 10,000 | Entries in one PDF object stream |
| `max_pdf_pages` | 10,000 | Pages in one PDF |
| `max_pdf_page_tree_depth` | 1,000 | PDF page-tree nesting |
| `max_pdf_reference_chain_depth` | 1,000 | Indirect-reference nesting |
| `max_pdf_reference_resolution_work` | 25,000 | Reference visits across one reader call, including indirect stream lengths and recovery candidates; cached lookups still count |
| `max_pdf_value_depth` | 100 | Nested PDF arrays and dictionaries |
| `max_pdf_decoded_stream_bytes` | 25,000,000 | One decoded PDF stream |
| `max_pdf_decompression_ratio` | 100 | Decoded-to-encoded stream ratio |
| `max_pdf_xref_length_candidates` | 1,000 | Candidate indirect `/Length` objects |
| `max_pdf_xref_revisions` | 1,000 | Incremental cross-reference revisions |

Reader limits apply across one operation, including nested parsing and decoding.
Repeated work counts even when results are cached. Separate calls to
`Reader.decoded_stream/2` have separate allowances.

Page sizes and transforms must fit the supported numeric range. Unrepresentable
calculations return a diagnostic; valid small nonzero scales are preserved.
PDF drawing imports share the reader's parsing and decoding limits.

Decompression and image predictors also need working buffers, so they can reach
a working-memory limit before the decoded-byte limit. These estimates are not
hard memory caps. The operation deadline and worker-memory allowance also apply.

## HTML processing and output

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_html_source_bytes` | 2,000,000 | One HTML source or advanced tree text node |
| `max_aggregate_html_source_bytes` | 10,000,000 | Body and all expanded furniture sources |
| `max_css_source_bytes` | 1,000,000 | One stylesheet or inline declaration source |
| `max_aggregate_css_source_bytes` | 20,000,000 | CSS source bytes prepared or parsed; shared prepared stylesheets are charged once, separate entries still count |
| `max_html_nodes` | 25,000 | HTML token processing steps across a render, and nodes in an advanced input tree |
| `max_html_depth` | 128 | Nested HTML elements or advanced input tree depth |
| `max_css_rules` | 10,000 | Stylesheet rules prepared across a render, including separate repeated entries |
| `max_css_work` | 5,000,000 | CSS scanned bytes, selector parsing work, and selector comparisons |
| `max_layout_boxes` | 100,000 | Drawing-box construction attempts, including measurements and furniture |
| `max_rendered_text_bytes` | 10,000,000 | Text production bytes, including generated-content parts before joining and transformations |
| `max_layout_text_work` | 20,000,000 | Text bytes measured and glyph-width entries visited, including repeated measurements |
| `max_layout_line_work` | 1,000,000 | Parts of newline-containing text runs constructed during layout, including empty parts and repeated measurements |
| `max_grid_occupied_cells` | 1,000,000 | Cumulative row-span times column-span occupancy insertions, including overlapping placements and repeated layouts |
| `max_grid_tracks` | 100,000 | Explicit and implicit row/column tracks constructed across grid containers and repeated layouts |
| `max_grid_placement_work` | 2,000,000 | Grid search candidates, candidate-cell reservations, intrinsic track scans, flexible sizing passes and track-prefix visits |
| `max_layout_measurement_cache_bytes` | 2,000,000 | Text bytes retained in one render's width cache; extra unique text is measured without caching |
| `max_layout_measurement_cache_entries` | 10,000 | Distinct width measurements retained in one render |
| `max_table_grid_work` | 1,000,000 | Table row scans, column expansion and probes, preferred-width span updates, and collapsed-border edge visits, reserved before expansion |
| `max_rendered_pages` | 1,000 | Generated PDF pages, also bounded by `max_pdf_pages` |
| `max_rendered_pdf_bytes` | 50,000,000 | Serialized PDF output and cumulative encoded link annotations; annotation bytes are admitted before copying URI data |

Repeated processing counts toward work limits even when the result is cached.
Nested rendering stages share an allowance. Calling a parser, layout or writer
separately starts a new one. Failures identify the affected limit in a diagnostic.

## HTML, images, and layout

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_png_working_bytes` | 160,000,000 | Estimated PNG scanline, reconstruction, output, and row-processing buffer allowance before decompression |
| `max_svg_nodes` | 25,000 | SVG XML elements |
| `max_svg_depth` | 128 | SVG element nesting, including the root |
| `max_svg_path_bytes` | 1,000,000 | Aggregate decoded `d` and `points` attribute bytes |
| `max_svg_filter_primitives` | 256 | SVG elements whose local name starts with `fe` |
| `max_svg_turbulence_octaves` | 16 | Maximum authored `numOctaves` per SVG `feTurbulence`, checked before native conversion; omitted values use 1 |
| `max_svg_filter_work` | 250,000,000,000 | Aggregate conservative pixel-neighborhood visits for SVG morphology and convolution filters, checked before native conversion |
| `max_svg_references` | 1,000 | SVG `href` attributes, including namespaced references such as `xlink:href` |
| `max_svg_output_bytes` | 10,000,000 | Returned SVG PNG bytes, checked after native conversion |
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

CSS limits include unused custom properties and expanded values. Calculations
outside the supported numeric range return a diagnostic with the expression.

PNG limits include color and alpha samples. A 16-bit RGBA image needs eight
sample bytes per pixel, plus decoding buffers. An image can reach the working
buffer limit before its decoded-image limit.

SVG structure limits apply before conversion; output image limits are checked
afterward. They do not cap native converter allocations. The turbulence limit
bounds each primitive's authored octave count, including fractional counts;
image size and repeated filters still affect total rendering cost.

The filter-work budget includes raster dimensions, transforms, filter reuse and
embedded SVGs. It estimates pixel count multiplied by neighborhood size, rather
than elapsed time or every CPU instruction. Some combinations with patterns,
markers, masks, filter images or indeterminate intermediate regions cannot be
bounded and return `:resource_limit_exceeded`. Simplify those combinations;
raising the limit cannot make an indeterminate estimate safe. Accepted SVGs still
run synchronously in the native converter, which operation deadlines cannot
interrupt. The budget does not bound every kind of native rendering work.

See [render options and supported formats](html-to-pdf-compatibility.md) and
[image processing and recovery](image-processing.md).

## Fonts

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_font_kerning_pairs` | 100,000 | Kerning subtables and glyph pairs inspected per font |
| `max_font_cmap_work` | 1,000,000 | Encoding records, segments, and codepoint visits prepared per TTF character map |
| `max_font_math_work` | 1,000,000 | Decoded MATH, selected math GSUB, family-name records/input bytes, and glyph-location/header bytes, plus explicit glyph IDs, variant/part searches and expanded geometry, coverage, and device entries per font-budget scope |
| `max_font_name_bytes` | 1,000,000 | Conservative UTF-8 decoding allowance across selected internal family-name ranges in one font-budget scope |
| `max_font_subset_work` | 100,000 | Glyphs and composite components visited while building a TrueType subset |
| `max_font_subset_bytes` | 40,000,000 | Padded output bytes across rebuilt TrueType subsets in one font-budget scope |
| `max_font_subset_checksum_work` | 10,000,000 | Padded 32-bit words checksummed across rebuilt subsets, including their directories |
| `max_font_cache_entries` | 64 | Parsed font files retained by the cache |
| `max_system_font_cache_entries` | 64 | Positive and negative installed-font discovery results retained by the cache |

Family-name decoding and font subsetting reserve space before expanding data.
Repeated output tables count separately even when they share source bytes.
One PDF writer call shares the subset allowances across its fonts.

Font tables must have unique tags and valid source ranges. Family names must
be nonblank UTF-8. Omitted weight and style use their defaults; explicitly
invalid values return a diagnostic.

See [font configuration](html-to-pdf-compatibility.md#fonts-and-text).

Standalone `Font.text_width/4`, `Font.css_text_width/4` and
`Font.css_glyph_advances/4` return the shared diagnostic error when measurement
exceeds its work allowance. Successful calls return numeric widths or glyph
advance lists as documented by each function.

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
its content again. See [Stamping](pdf-stamping.md).

## Merging and splitting

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_merge_inputs` | 100 | PDFs in one merge |
| `max_assembled_pdf_bytes` | 50,000,000 | Each merged, transformed, or split output, also bounded by `max_pdf_input_bytes` |
| `max_aggregate_merge_input_bytes` | 100,000,000 | PDF bytes in one merge |
| `max_merged_objects` | 100,000 | Objects in merged output |
| `max_merged_pages` | 10,000 | Pages in merged output, also bounded by `max_pdf_pages` |
| `max_split_outputs` | 1,000 | PDFs returned by one split operation |
| `max_split_object_writes` | 1,000,000 | Aggregate objects serialized by one split operation |
| `max_aggregate_split_output_bytes` | 100,000,000 | Aggregate bytes returned by one split operation |

Merge limits apply across the combined input/output. Split byte and object
limits apply across all returned PDFs. See [Merging](pdf-merging.md) and
[Splitting](pdf-page-transforms.md#splitting).

## Text extraction

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_extracted_text_bytes` | 50,000,000 | Aggregate UTF-8 text and replacement strings prepared across all pages and Form executions |
| `max_cmap_expanded_bytes` | 10,000,000 | Aggregate expanded ToUnicode mapping keys and UTF-8 destinations, including overwritten entries |
| `max_text_decoded_content_bytes` | 50,000,000 | Decoded content used by extraction |
| `max_text_parsed_instructions` | 100,000 | Unique parsed content instructions |
| `max_text_parsed_tokens` | 1,000,000 | Tokens consumed while parsing unique content sequences, including operators and container delimiters |
| `max_text_parsed_values` | 500,000 | Operand values and opened containers parsed across unique content sequences |
| `max_text_font_work` | 1,000,000 | Aggregate uncached fonts, expanded width entries and simple-font Differences entries prepared during extraction |
| `max_text_stream_uses` | 100,000 | Content stream references traversed |
| `max_text_instruction_uses` | 1,000,000 | Aggregate content instruction work |
| `max_text_form_expansions` | 10,000 | Form XObject executions |
| `max_text_spans` | 25,000 | Non-empty decoded text operands per page, including pieces suppressed by replacement text |
| `max_text_marked_content_depth` | 128 | Nested BMC/BDC marked-content scopes |
| `max_text_layout_whitespace_bytes` | 1,000,000 | Coordinate-based padding spaces reconstructed across one text extraction |
| `max_cmap_work` | 1,000,000 | Aggregate CMap source bytes, expanded declarations, and conservative codespace lookup work across extraction |
| `max_cmap_bytes` | 1,000,000 | One CMap stream |
| `max_cmap_entries` | 100,000 | Mappings in one CMap |
| `max_cid_width_entries` | 65,536 | Width declarations expanded for one CID font, including overwritten entries |
| `max_form_xobject_depth` | 20 | Form XObject nesting |

Reconstructed whitespace counts added padding in `layout: true` output.
Parsing and instruction-use limits apply across pages and reused content.
The PDF CID range is fixed at 0 through 65,535, so `max_cid_width_entries`
cannot exceed 65,536. See [Text extraction](text-extraction.md).

## Check active settings

`NativeElixirPdfUtilities.Limits.defaults/0` returns built-in values.
`NativeElixirPdfUtilities.Limits.effective/0` returns the active configuration.

## Initial HTML form values

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_html_pattern_bytes` | 65,536 | HTML pattern source bytes across a render |
| `max_html_form_value_bytes` | 1,000,000 | URL sources and checked initial values across a render |
| `max_html_pattern_values` | 10,000 | Initial values checked across a render |
| `max_html_pattern_compile_work` | 2,000,000 | Pattern parsing and expansion work |
| `max_html_pattern_match_work` | 5,000,000 | Pattern matching work across a render |
| `max_html_pattern_depth` | 128 | Pattern and matching nesting |
| `max_html_form_value_memory_bytes` | 32,000,000 | Memory for pattern and URL checks |
| `max_html_form_value_timeout_ms` | 5,000 | Milliseconds allowed for one checking request |

Pattern and URL checks run in a separate worker. A stopped or memory-killed
worker returns a diagnostic. The memory check does not provide an instantaneous
allocation ceiling or cover native regex allocations.

## Forms and attachments

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_form_fields` | 10,000 | HTML controls and visited field/widget nodes |
| `max_pdf_form_depth` | 64 | AcroForm field-tree nesting |
| `max_pdf_form_text_bytes` | 1,000,000 | Field names and filled text values |
| `max_pdf_form_metadata_bytes` | 10,000,000 | Aggregate expanded field names, decoded choice labels/values, appearance-state names and retained editor labels/font-family names during inspection |
| `max_pdf_form_preparation_entries` | 100,000 | Aggregate widget references, choice/selection entries, appearance states, retained list-box rows and their scalar paint facts, editor descriptors and projection coordinates prepared during inspection |
| `max_pdf_form_font_uses` | 100,000 | Editor font descriptors visited, including uses of cached streams |
| `max_pdf_form_font_work_bytes` | 1,000,000,000 | Sum of font-program bytes referenced by inspected editor descriptors, including repeated uses |
| `max_pdf_attachments` | 1,000 | Total embedded-file count |
| `max_pdf_attachment_bytes` | 10,000,000 | One new attachment's bytes |
| `max_pdf_attachment_total_bytes` | 25,000,000 | New payload bytes plus existing stored attachment stream bytes |
| `max_mime_container_bytes` | 10,000,000 | ZIP bytes inspected for MIME evidence |
| `max_mime_container_entries` | 10,000 | ZIP central-directory entries inspected |

Form inspection counts expanded names, option rows, metadata and font uses.
Repeated font references still consume work allowances even when their bytes
are cached. Filling and flattening include an inspection. Form appearances also
use the existing object, layout and font limits.

Attachment metadata uses `max_pdf_info_value_bytes`. Existing attachment streams
count as stored, without decompression; declared uncompressed sizes are metadata.
The original PDF also counts toward the updated document's output-byte limit.

## Appearance expansion and incremental output

`max_appearance_text_bytes` defaults to 5,000,000 bytes across form and stamp
appearances. Repeating text on multiple pages or widgets charges each copy.
`max_appearance_widgets` defaults to 10,000 generated appearances or selected
widgets; HTML controls reserve both their normal and alternate appearances.
Appearances have their own limits and do not count as document pages.
`max_rendered_pages` and `max_pdf_pages` still bound the actual PDF. Layout,
font, reader, object and output-byte limits also apply.

Updated PDFs must fit both `max_pdf_input_bytes` and `max_rendered_pdf_bytes`.
The limit includes the original PDF and the appended revision. Other reader
limits still apply.

Incremental updates reserve space in `max_pdf_xref_revisions` before writing.
A form fill with `flatten: true` reserves two revisions. Empty updates that
return the original PDF do not consume another revision.

## Font sources and caches

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_font_source_bytes` | 10,000,000 | One configured, document-selected, bundled, or discovered font |
| `max_aggregate_font_source_bytes` | 40,000,000 | Distinct font byte payloads used during one render, including furniture |
| `max_font_count` | 128 | Family/weight/style combinations, including bundled fallbacks |
| `max_font_candidates` | 256 | Font source lookup and parse attempts, source candidate lists and default-family lists; repeated file snapshots reuse the render-local result |
| `max_font_discoveries` | 64 | Distinct system discovery requests in one render, including process-cache hits |
| `max_font_cache_bytes` | 100,000,000 | Retained font-file cache entries |
| `max_system_font_cache_bytes` | 100,000,000 | Retained system-discovery cache entries |

Configured font files, supplied bytes and resolved assets use the same font
limits. Repeated sources can be reused within a render. Shared caches also have
entry-count and memory limits; resource failures are not cached.

System discovery reads trusted installed fonts through its native dependency,
which returns complete bytes rather than a file handle or path. Request count
and returned bytes are bounded, but this API cannot enforce the source-byte
limit before that native allocation. Applications that need to control every
font read should set `system_font_discovery: false` and supply approved font
bytes or files. Resolver callbacks likewise control their own I/O before
returning bytes. None of these per-render settings imposes a global concurrency
or native-memory cap.

## File input and local assets

HTML, CSS and PDF file reads accept regular files within their source-byte
limits. Image and font reads also respect the remaining aggregate allowance.
File type and size are checked before opening and on the opened handle.

`:base_url` rejects path traversal and existing symlinks beneath its directory.
Keep paths and asset directories under trusted control, or supply approved bytes
through `:assets` or `:asset_resolver`. Concurrent filesystem changes can race
these checks. Replacing a file with a FIFO during opening can still block;
byte limits do not impose an I/O timeout.
