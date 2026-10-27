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

Asset resolvers run in the worker. Capture the caller PID when callbacks need
to send it messages; caller process dictionary entries are not inherited.

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
| `max_pdf_container_entries` | 100,000 | Entries in one array or dictionary, including repeated dictionary keys |
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

Reader budgets span the complete synchronous operation, including nested probes
and candidate recovery. A failed candidate does not refund its work. Exhaustion
returns a diagnostic and releases the budget before another call starts. Stream
decoding requested separately through `Reader.decoded_stream/2` gets a fresh
budget; callers repeatedly decoding content still need their operation's own
aggregate limit, as text extraction already has. Flate output is drained in
bounded chunks, and expanding filters stop at the remaining decoded allowance.
Reference caches retain terminal identities and chain depths, while resolved
values remain in the document object table. Separate object tables and recovery
candidates have separate caches but share an active reader's reference allowance.
Standalone validator calls retain their own bounded resolution scopes.

Geometry validators admit derived scales, translations and complete placement
matrices before writing. Unrepresentable arithmetic returns a contextual
diagnostic. Serialized coefficients also obey the shared numeric magnitude and
token-byte limits; representable small nonzero scales retain their precision.
Intermediate appearance bounds may exceed the output magnitude limit when the
final validated placement remains within it.

PDF Form drawing imports share one reader allowance across source preparation
and decoded content occurrences, including joining newlines. The writer reuses
the validator's prepared import for identical sources. Encoded single-stream
Forms retain their original filters and bytes. Overlay resource collection visits
each unique source page before expanding placements, charges visited values to
`max_pdf_reader_work`, and charges newly followed indirect references to
`max_pdf_reference_resolution_work`.

Predictors process packed rows and predecessor pixels without per-sample maps.
Their buffer allowance counts the input bytes, twice the output bytes, two row
buffers and two pixel buffers. Empty predictor input requires no reconstruction
buffers. This is conservative buffer accounting, not an exact BEAM heap or
native-memory measurement. TIFF sample/pixel visits and PNG byte/group/filter
visits are charged before reconstruction; invalid row dimensions, incomplete
rows and unsupported filter bytes return diagnostics. LZW clear codes reuse the
fixed initial dictionary and consume work even when they produce no output.

These counters bound parsing work and retained structures. Document API calls
also have the operation deadline and worker-memory allowance above. Neither
provides an application-wide memory ceiling.

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

Work counters charge repeated processing rather than just retained results.
Reaching a limit aborts the operation with an actionable diagnostic and clears
its budget. Standalone parser, style, layout, pagination, furniture, and writer
calls create their own budget. Nested pipeline stages share the active budget.
CSS source validation retains its `:invalid_css` diagnostic contract; other new
render limits return `:resource_limit_exceeded`.

## HTML, images, and layout

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_png_working_bytes` | 160,000,000 | Estimated PNG scanline, reconstruction, output, and row-processing buffer allowance before decompression |
| `max_svg_nodes` | 25,000 | SVG XML elements |
| `max_svg_depth` | 128 | SVG element nesting, including the root |
| `max_svg_path_bytes` | 1,000,000 | Aggregate decoded `d` and `points` attribute bytes |
| `max_svg_filter_primitives` | 256 | SVG elements whose local name starts with `fe` |
| `max_svg_turbulence_octaves` | 16 | Maximum authored `numOctaves` per SVG `feTurbulence`, checked before native conversion; omitted values use 1 |
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

CSS budgets include unused custom properties. Dependencies are memoized during
each custom-property computation, and output sizes are checked before building
expanded binaries. Cycles and missing variables retain their existing invalid-value
behavior. A limit failure returns `:resource_limit_exceeded` at the `:limits` stage.

CSS calculations also reject finite arithmetic that overflows the runtime's
floating-point range, including intermediate results during dimension checking.
This returns `:resource_limit_exceeded` at the `:style` stage with the expression
and guidance for reducing it. The runtime numeric range is a fixed invariant,
not a configurable resource limit. Small nonzero values remain accepted when
their calculations fit the supported range. Explicit CSS infinity and NaN keep
their existing calculation semantics.

PNG decoded-image budgets count bytes per sample, including alpha when present.
A 16-bit RGBA image reserves eight bytes per pixel before decoding. The working
allowance also covers inflated scanlines and reconstruction buffers. It can reject
an image below the decoded-image limit; raising one limit does not bypass another.
The estimate reserves three times the inflated bytes, three times the final sample
bytes, 64 times the largest packed row, and 64 bytes per pass row. These allowances
account for retained buffers and row-processing overhead, not a hard BEAM memory cap.

SVG structure limits apply before Resvg conversion. They bound input structure,
not expanded reference work or native allocations. SVG output is checked after
conversion and then validated and decoded through the PNG pipeline. Returned
converter errors become diagnostics, but native resource exhaustion and all
renderer warnings cannot be recovered through that contract.

The turbulence octave limit rejects oversized requests with a
`:resource_limit_exceeded` diagnostic. Counts must be finite nonnegative unitless
numbers within the shared numeric bounds. The limit applies to the authored
number before converter rounding, including fractional counts. It bounds one
primitive's octave count, not total filter CPU time; image size, repeated artwork
and filter count still affect rendering cost. It does not change native converter
scheduling.

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

Family lookup selects needed records before decoding and reuses identical string
ranges. Each unique decoded range also charges its input bytes to
`max_font_math_work`. The name-byte allowance reserves up to twice the UTF-16
input size or three times the MacRoman input size before decoding.

Subset preparation admits all padded output bytes and checksum words before
copying glyphs, assembling tables or checksumming payloads. Distinct table tags
may share source storage; each output payload occurrence counts separately.
One PDF writer call shares these allowances across its subset fonts.

Loading, glyph metrics and subsetting share the same table-directory policy.
This library requires unique table tags and ranges that follow the directory
and fit within the source font, including opaque custom tables.
Configured family names and default families must be nonblank UTF-8. Omitted
weight and style use their defaults; explicitly invalid values return a diagnostic.

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
its content again. See [Stamping](pdf-stamping.md).

## Merging and splitting

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_merge_inputs` | 100 | PDFs in one merge |
| `max_assembled_pdf_bytes` | 50,000,000 | Each merged, transformed, or split output, also bounded by `max_pdf_input_bytes` |
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

Reconstructed whitespace counts added padding, not original text or page
separators. It applies only to `layout: true`. The PDF CID range is fixed at
0 through 65,535, so `max_cid_width_entries` cannot exceed 65,536.
Token, value and instruction admission happens while parsing, before retaining
the next item. These counters span pages and Forms; cached content reuses its
parsed values and still counts toward instruction-use limits. Container nesting
uses `max_pdf_value_depth`. Font work is admitted before expanding widths or
Differences. Indirect fonts reuse their prepared encodings, and Type0 fonts
sharing the same indirect descendant reuse its width map.
See [Text extraction](text-extraction.md).

## Check active settings

`NativeElixirPdfUtilities.Limits.defaults/0` returns built-in values.
`NativeElixirPdfUtilities.Limits.effective/0` returns the active configuration.

## Initial HTML form values

Pattern and URL checks run in a disposable BEAM worker with an external deadline.
`max_html_pattern_compile_work` charges syntax and property expansion.
`max_html_pattern_match_work` is shared by all values in a request and across the
render. Native assertion matching charges transitions and copied/scanned bytes.
OTP does not expose successful regex step counts, so each run debits its entire
granted engine allowance, including unsuccessful retries. A token never receives
a fresh full allowance. `max_html_pattern_depth` bounds syntax and matching depth.

`max_html_form_value_memory_bytes` checks observed worker memory before and after
execution, checks expanded expressions, and configures a GC-triggered BEAM heap
cap including shared binaries. BEAM checks that cap at garbage collection, with
its platform minimum heap size. This is not an instantaneous allocation ceiling
and excludes native regex allocations. An observed excess returns the named
resource diagnostic. A GC heap kill and an unexpected external kill have the
same exit reason, so both return the worker failure diagnostic with heap-limit
advice. Caller flags and unrelated mailbox messages are preserved.
`max_html_form_value_timeout_ms` independently terminates a stalled worker.

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

Form appearances also use `max_pdf_objects` and the existing font limits.
Form inspection checks expanded names before joining parent/child names and
checks metadata and entry allowances before retaining prepared collections.
Decoded editor fonts are cached by their resolved stream reference within one
inspection. The PDF reader's aggregate decode/work limits apply across uncached
font streams; repeated descriptors still consume font-use and logical font-byte
work allowances. These work bytes are not a measure of retained memory. Each
inspection starts a fresh scope, including the inspection preceding fill or
flatten.
Retained list-box rows also consume the metadata and preparation-entry
allowances across all controls while a producer serializes their editor
templates.

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
These limits are checked before appearance rendering. Native appearance frames do not count as document pages; `max_rendered_pages` and `max_pdf_pages` still bound actual PDF pages. Appearance resources retain the existing layout, font, reader, object and output-byte budgets.

The shared incremental writer checks serialized pieces as they accumulate. All
incremental writers also check the complete iodata before
flattening it. Output must fit both `max_pdf_input_bytes` and
`max_rendered_pdf_bytes`, so an edit cannot return a document already too large
for the reader under the current byte limit. Other reader limits still apply.

Incremental updates reserve space in `max_pdf_xref_revisions` before writing.
A form fill with `flatten: true` reserves two revisions. Empty updates that
return the original PDF do not consume another revision.

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

The reader also checks file type and size before opening the path, so an
existing FIFO is rejected without waiting for a writer. The opened-handle
checks remain necessary because a file can change after the pathname check.

Local asset reads use the same portable reader on Windows, macOS, and Linux.
No Python installation is required. `:base_url` rejects traversal and existing
symlinks beneath the base directory before opening a file. These pathname
checks cannot prevent concurrent filesystem changes. Keep file paths and asset
directories under trusted control, or supply approved bytes through `:assets`
or `:asset_resolver`. In particular, replacing a regular file with a FIFO between
the pathname check and opening can still block. The portable Erlang file API
does not provide a nonblocking-open option. Byte limits do not impose an I/O
timeout.
