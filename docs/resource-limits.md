# Resource limits

The library uses twenty-one shared settings, including three rendering precision
options. Features and loops reuse the same budgets instead of introducing their own configuration keys. A reached budget
returns an actionable [diagnostic](diagnostics.md), usually with reason
`:resource_limit_exceeded`.

## Configuration

Override only the settings you need in `config/config.exs` or
`config/runtime.exs`, then restart the application:

```elixir
config :native_elixir_pdf_utilities,
  limits: [
    max_input_bytes: 75_000_000,
    max_operation_timeout_ms: 300_000,
    max_operation_memory_bytes: 1_073_741_824
  ]
```

Settings apply across the application and cannot be overridden per call.
Unknown, repeated or invalid settings prevent startup. Values must be positive
integers. Timeouts and the memory-check interval cannot exceed 4,294,967,295
milliseconds, the BEAM runtime's fixed maximum.

## Operation allowance

| Setting | Default | Resource |
| --- | ---: | --- |
| `max_operation_timeout_ms` | 300,000 | Deadline for the complete operation, including nested work and form-pattern evaluation |
| `max_operation_memory_bytes` | 1,073,741,824 | Observed BEAM worker memory and referenced binaries; also the ceiling for estimated working buffers |
| `operation_memory_check_interval_ms` | 25 | Interval between worker-memory checks |

Public document operations run in disposable workers. Nested calls share the
operation's deadline and memory allowance. Starting another loop or processing
another format does not create a new allowance. Applications control concurrent
operations. Form-value helpers check their own observed working set against the
same memory setting. Sequential helper peaks are not added after their workers
have exited.

Observed excess returns `:resource_limit_exceeded`. Unexpected worker exits,
including GC heap kills, return `:operation_worker_failed`. Sampling can miss
peaks and excludes native allocations, persistent caches and helper processes.
Native work may continue after a timeout. Use OS process or container isolation
when hard memory or cancellation guarantees are required.

Resolvers execute in the requesting worker. See
[resolver process context](html-to-pdf-compatibility.md#resolver-process-context)
for caller-local data and messaging behavior. Font-cache misses execute in that
worker too; cancelling it stops its loader without blocking unrelated misses.

## Shared processing budgets

| Setting | Default | Resource |
| --- | ---: | --- |
| `max_input_bytes` | 50,000,000 | Supplied document, stylesheet, font, image, attachment, dictionary and other source bytes |
| `max_output_bytes` | 50,000,000 | Generated PDF, split output, extracted/generated text and converted image bytes |
| `max_decoded_bytes` | 100,000,000 | Decoded streams, images, fonts, expanded maps, CSS substitutions and extracted metadata |
| `max_entries` | 1,000,000 | Parsed values, tokens, nodes, glyphs, fields, collections, intermediate layout entries and temporary cache entries |
| `max_depth` | 1,000 | Recursive document, CSS, font, PDF, reference, embedded-content and drawing nesting |
| `max_work` | 1,000,000,000 | Scanned bytes, repeated parsing, lookups, substitutions, reference visits, layout searches and other explicitly counted processing steps |
| `max_pages` | 10,000 | Input, generated and assembled document pages |
| `max_numeric_magnitude` | 4,294,967,295 | Absolute numeric operands and derived coordinates |
| `max_numeric_token_bytes` | 1,024 | Numeric text admitted before integer or floating-point conversion |
| `max_decompression_ratio` | 100 | Decoded-to-encoded PDF stream ratio |
| `max_cache_bytes` | 100,000,000 | Retained font, discovery and layout measurement cache data |
| `max_cache_entries` | 64 | Entries retained in a persistent cache between operations |

A shared ceiling bounds both individual collections and cumulative resource
accounting. Nested rendering, font preparation and PDF parsing reserve from the
same active budget. Repeated work counts even when a result is cached. Standalone
preparation starts a budget when no enclosing one exists. Cache-retention checks
apply to the cache's lifetime; exceeding them evicts entries rather than failing
an otherwise valid document.

Persistent caches need a separate retained-entry ceiling because their metadata
outlives operation workers, including negative discovery results with tiny byte
payloads. All persistent cache types reuse the same setting. Temporary layout
measurements use the operation's shared entry allowance instead.

Input, output and decoded bytes describe different resources. A compressed
source can fit the input allowance while exceeding its decoded allowance.
Intermediate filter output counts too. Generated PDFs must also fit the input
ceiling so the library can read its own output. Splitting checks cumulative
output before retaining another result.

Counts and work estimates are deterministic guards, not measurements of CPU time
or exact heap usage. A larger byte budget does not remove the entry, depth, work,
deadline or memory checks. Failure diagnostics identify the active setting and
include the operation, format or source when that information is available.

Format bounds remain fixed independently of configuration. For example, PDF CIDs
cannot exceed 65,535, glyph IDs remain unsigned 16-bit values, and HTML table
spans retain their specified maxima. Raising `max_entries` does not change those
bounds.

## Raster resources and precision

| Setting | Default | Resource |
| --- | ---: | --- |
| `max_raster_dimension` | 8,192 | Raster width or height |
| `max_raster_pixels` | 134,217,728 | Raster pixels and estimated intermediate surfaces across rendering |
| `max_raster_work` | 250,000,000,000 | Estimated native pixel-neighborhood visits for SVG filters |
| `svg_raster_scale` | 2 | Raster samples per CSS output unit |
| `clip_curve_precision` | 1,024 | Reciprocal of maximum clipping curve-to-chord deviation in PDF points |
| `font_color_sweep_segments` | 2,048 | Sweep-gradient approximation segments for color glyphs |

Raster work has a separate setting because native pixel-neighborhood estimates
use different units from parser and layout steps. Raster dimensions, pixels and
estimated working buffers are checked before conversion where their sizes can
be determined. PNG estimates include scanlines, reconstruction, output and row
buffers. A 16-bit RGBA sample needs eight decoded bytes per pixel.

SVG estimates include transforms, reused filters and embedded content. Some
combinations involving patterns, markers, masks, filter images or indeterminate
intermediate regions cannot be bounded and return `:resource_limit_exceeded`.
Increasing a setting cannot make an indeterminate estimate safe. Accepted SVGs
still run synchronously in the native converter, and these checks do not cap all
native allocations or rendering work.

## Migrating feature-specific settings

Earlier feature-specific keys have been removed. Replace them with the shared
setting for the resource they controlled. Font, table, grid, CSS-variable and
line-processing work use `max_work`; their collection counts use `max_entries`.
All nesting controls use `max_depth`. Font/discovery/cache byte ceilings use
`max_cache_bytes`; input and decoded byte ceilings use their corresponding shared
settings. Dictionary preparation uses the same source, entry and work allowances
as other text processing.

The consolidation changes defaults as well as names. Review application overrides
and the diagnostic for a rejected document when migrating. The current accepted
keys and defaults are available through `NativeElixirPdfUtilities.Limits.defaults/0`.
