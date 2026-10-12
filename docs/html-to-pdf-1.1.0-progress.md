# HTML-to-PDF 1.1.0 implementation progress

The milestone starts with parsing and validation, then static HTML, styles and
resources, controls and composition, inline SVG, embedded documents, and MathML.
This file records delivered batches and remaining work. The full milestone is
not complete.

The earlier batch 55 handoff was resumed on 2026-10-12. Five combined integration
waves now pass the required gates. The latest copied integration is
`38333b9235899696e86052f80915d0c6b37bfe3c`. Later accepted lane checkpoints are
being combined in Wave 6. Historical batch entries below retain their original
acceptance evidence and remaining scope. The milestone remains unfinished.

## Batch 1: comments and attribute syntax

The parser accepts comments in ordinary HTML content and empty, unquoted,
single-quoted, and double-quoted syntax for supported attributes. Comments paint
nothing and do not add whitespace. Raw text and RCDATA retain literal comment
delimiters. Unquoted values preserve slashes and decode character references in
attribute context. Attribute separators use ASCII whitespace. Duplicate names
remain case-insensitive errors, and unsupported attributes remain errors.

`Validators.HtmlValidator` owns attribute validation and comment delimiter rules.
The parser uses its validated attributes and comment results. Existing parser
structure checks still need to move into validators in later parsing batches.
Tokenizer mode detection avoids parsing ordinary attributes twice.

### Source records

The source is the HTML Standard snapshot dated 2026-10-05. The related inventory
records remain partial while the broader valid-syntax work is unfinished.

| Source group | Records | Delivered behavior and remaining scope |
| --- | --- | --- |
| [Attributes](https://html.spec.whatwg.org/#attributes-2) | `section:attributes-2`, `definition:syntax-attributes`, `definition:syntax-attribute-value`, `requirement:1391` through `requirement:1398` | Four value syntaxes, separators, unquoted slash handling, references, and duplicate rejection for supported attributes. Broader attribute names, foreign attributes, and further static semantics remain. |
| [Comments](https://html.spec.whatwg.org/#comments) | `section:comments`, `definition:syntax-comments`, `algorithm:2308` | Comment boundaries and body delimiter restrictions, including empty comments, quotes, angle brackets, nesting rejection, and unterminated-input diagnostics. Document construction, foreign-content contexts, and valid empty documents remain. |
| [Start tags](https://html.spec.whatwg.org/#start-tags) | `section:start-tags`, `algorithm:2303` | Attribute syntax uses ASCII separators and preserves a slash that belongs to an unquoted value. Complete element and namespace rules remain. |

### Acceptance evidence

- Parser tests cover empty and quoted values, ASCII separators, case handling,
  references, slashes, duplicate names, malformed attributes, comment contexts,
  raw text, RCDATA, and invalid executable attributes on raw-text elements.
  Diagnostics locate the failing token even when its text appears earlier in
  comments or quoted metadata.
- Public rendering tests check visible extracted text, actionable failure
  diagnostics, source locations, and the shared HTML token budget for comments.
- `html_comments_attributes.html` compares text, generated content, table cells,
  and two pages against Chromium at the existing pixel thresholds.
- The performance script retains the existing workloads and thresholds. Optional
  JSON reports add individual timed samples, input hashes, runtime details, and
  three separate sampled-memory runs for each workload. The quality matrix
  retains these reports beside its runtime logs.

The full quality matrix passes on Elixir 1.19 and 1.20 with 100% coverage and no
warnings. The required host Chromium parity run passes all 87 tests. The new
fixture has two pages and a maximum changed-pixel ratio below 0.035%.
The inventory audit accounts for all 17,267 records without missing or duplicate
IDs. No broad inventory rows moved to implemented.

### Performance evidence

Fixed 1.0.0 library measurements live in
`.quality/performance/baselines/1.1.0/reports.json`.
Batch 1 reports in `.quality/performance/results/1.1.0-batch-1/reports.json` retain the candidate's
timings, reductions, sampled memory, input and library hashes, and PDF sizes for
the next batch. All 19 workloads have matching input hashes and unchanged output
sizes within each runtime. Existing guardrails pass without changing thresholds.

| Workload | Elixir 1.19 reduction change | Elixir 1.20 reduction change |
| --- | ---: | ---: |
| Invoice | -0.32% | -0.92% |
| 100-row HTML render | -1.76% | -3.10% |
| 160 table headings | -1.92% | -0.49% |

Profiling identified unnecessary regex work for empty attributes. The fast path
reduced parser-only reductions on the 160-table input from about 411,000 to
335,000. Wall-clock comparisons remain provisional because captures varied substantially
while unrelated builds ran, including in unchanged PDF-only operations.
A quiet-host comparison is still required before release acceptance. Sampled VM
memory is recorded rather than treated as an exact peak or a proven improvement.

The separate syntax experiment renders 1, 100, and 1,000 paragraphs with empty
and unquoted attributes and two comments per paragraph. Equivalent markup uses
quoted attributes and the same visible text, with bundled fonts and system font
discovery disabled. Visual text extraction matches for every size. For 1,000
paragraphs, the equivalent markup uses about 12.6 million reductions and produces
102,335 bytes; the new syntax uses about 13.9 million reductions and produces
106,285 bytes. This extra feature cost is separate from unchanged workloads.

## Batch 2: optional tags and document construction

The renderer now constructs omitted `html`, `head`, `body`, `tbody`, and
`colgroup` elements. CSS child and descendant selectors see this ancestry.
`HtmlParser.parse/1` and `parse_detailed/1` retain their original tree format.
The new `parse_document_detailed/1` exposes the constructed tree;
`parse_fragment/2` and `parse_fragment_detailed/2` accept supported contexts such
as table rows, select elements, paragraphs, raw text, and RCDATA.

Supported paragraph, list-item, option, caption, column-group, table-section,
row, and cell end tags can be omitted according to validator rules. Explicit
closing tags also work when supported document/table start tags are omitted.
Required missing end tags, duplicate sections, misplaced doctypes, and invalid
table-section order return diagnostics. Plain text and inline content can occur
at the root or directly in the body. Empty documents produce a blank page;
empty tables, column groups, row groups, and rows are valid. A leading UTF-8 BOM
is transport metadata. UTF-8 remains the input contract; an HTML doctype and
`SYSTEM "about:legacy-compat"` are supported, with quirks/XML modes diagnosed.

All former parser element support, content-model, and element-semantic checks
now live in `HtmlValidator`. It also owns omission, doctype, fragment-context,
and text-content rules. Table groups are constructed during parsing, avoiding
another traversal of the whole document. Source diagnostics use original token
positions, including after a BOM and inside fragments.

The two-page Chromium fixture exposed existing list differences. List content
now begins at the authored indentation, markers sit outside it, unordered
markers use bullets, and items have no extra default bottom margin. List defaults
use the browser's 40px indentation and font-relative block margins.

### Acceptance evidence

The matrix at `.quality/logs/20261006T083211Z-1686645/` passes on both runtimes
with 100% coverage, no warnings, and unchanged performance guards. It runs 1,015
ordinary tests and 88 Chromium comparisons per runtime. The required host parity
command also passes all 88 comparisons. Parser/public tests check equivalent
explicit/omitted trees, preserved legacy trees, fragment diagnostics, empty
structures, table order, source coordinates, and empty-heading outline behavior.

Related source records include `section:optional-tags`, `requirement:1400`
through `requirement:1423`, and the fragment-parsing algorithms. Inventory rows
remain partial while further element/context, foreign-namespace, and complete
construction requirements remain. The inventory audit still accounts for all
17,267 IDs, with no category changes or lost records.

### Performance evidence

Batch 2 reports in `.quality/performance/results/1.1.0-batch-2/reports.json` retain the same 19
workloads and input hashes as the fixed baseline and batch 1. Output sizes match
except `trim_card.html`, which adds 12 bytes to paint its authored body background
now that the implied body participates in layout. Both runtimes pass the original
reduction, timing, and output-size ceilings.

| Workload | 1.19 reductions vs fixed / batch 1 | 1.20 reductions vs fixed / batch 1 |
| --- | ---: | ---: |
| Invoice | -0.39% / -0.06% | -0.96% / -0.03% |
| 100-row render | -0.94% / +0.83% | -1.44% / +1.71% |
| 160 table headings | -0.87% / +1.07% | -0.70% / -0.22% |

Constructed document/table ancestry adds required selector and style work.
Avoiding a second whole-tree construction pass removes unnecessary work.
Timing comparisons against earlier noisy captures remain provisional until a
controlled paired run is available. Reports retain separate sampled VM memory.

A separate scaling experiment compares explicit and omitted table groups/end
tags for 1, 100, and 1,000 tables, with two warmups and five samples. PDF bytes
and extracted text are identical in each pair. At 1,000 tables, explicit syntax
uses about 29.5 million reductions and omitted syntax about 28.7 million;
both produce 100,398 bytes. These timings are exploratory, separate from the
unchanged-workload guardrails.

## Batch 3: static text, descriptions, figures, and nested lists

The renderer accepts `address`, `blockquote`, `figure`, `figcaption`, `hgroup`,
`search`, description lists and their groups, and supported semantic inline text.
Legacy `center`, `listing`, and `tt` retain their static appearance. New element
content-model checks, description groups, caption positions, and descendant
restrictions live in `HtmlValidator`. Description items support their standard
omitted end tags. Metadata values remain available to selectors without adding
printed text.

List items accept mixed inline/block content and nested lists. Nested unordered
lists use circle and square markers. Preformatted text preserves spaces, default
eight-space tab stops, and newlines across inline descendants. One initial
newline is removed for `pre`, `listing`, and `textarea`; a terminal newline does
not add a blank final line. CSS supports `pre`, `pre-wrap`, and `break-spaces`.
Pinned unmodified DejaVu Sans Mono 2.37 faces load only when selected. Their
complete upstream notice and file hashes are retained with the fonts.

Browser comparisons required correcting heading default margins, parent/child
margin collapse, and child placement after zero-height forced page-break markers.
Author CSS still overrides semantic defaults. The two-page
`html_static_elements.html` fixture covers these changes with identical fonts
in both renderers and the existing one-percent per-page pixel limit.

### Acceptance evidence

The final matrix at `.quality/logs/20261006T091035Z-1818594/` passes Elixir 1.19
and 1.20 with 100% coverage, no warnings, all existing performance guards, and
89 Chromium comparisons per runtime. The normal suite runs 1,024 tests.
The later batch 4 host parity run passes all 90 comparisons, including the
optimized validator and this fixture.
Related element/section source IDs moved from planned to partial, preserving
17,267 unique records. No broad rows moved to implemented. Complete attributes,
composition, directional-layout dependencies, and feature performance acceptance
remain attached to their broader inventory rows.

### Performance evidence

Batch 3 reports in `.quality/performance/results/1.1.0-batch-3/reports.json` retain matrix reports,
two opposite-order captures of the fixed baseline, batch 2, and batch 3, plus
longer repeated measurements. The supplemental baseline uses the unmodified
1.0.0 library with the current unused font assets and report-enabled harness,
so all benchmark input hashes match. Original fixed baseline records remain
unchanged. Parser ancestor rules now test the applicable tag first, avoiding
unnecessary scans for ordinary tags.

Existing output sizes match except the prior 12-byte `trim_card.html` background
correction and a one-byte change in the synthetic table outputs after correcting
its heading geometry. Uncompressed output decreases one byte. Existing limits
and fixtures remain unchanged. Timing investigation and the final controlled
comparison are still being recorded; passing the guards does not approve an
unexplained slowdown.

Separate new-feature measurements retain raw elapsed/reduction samples, three
sampled-memory runs, output bytes, options, and source/library hashes. At 1,000
description/preformatted blocks the median is about 28.4 million reductions,
183,277 PDF bytes, and a sampled VM peak about 149 MB. Nested-list depth scales
through 30 levels with retained content. The report distinguishes the first
fresh-VM workload from later font-cache reuse. Native allocations and brief
memory peaks are outside the sampled VM measurement.

## Batch 4: list attributes and styled markers

Ordered lists implement `start`, `reversed`, item `value`, and case-sensitive
legacy type hints. Unordered lists and items retain their supported legacy
hints. HTML integer parsing consumes an optional sign and leading digits after
ASCII whitespace, with a shared configurable numeric-token byte limit before
conversion. Invalid integer syntax uses the element's default. Invalid or
unsupported CSS marker values return diagnostics.

Supported CSS marker types include decimal, decimal-leading-zero, alphabetic,
Roman, disc, circle, square, and none. Inside markers wrap with the content;
outside markers align with its first baseline. Markers receive the selected
font and glyph fallback. Item padding, borders, backgrounds, and pagination
use the ordinary block machinery. Hidden and non-list-item children do not
advance ordered numbering. General CSS-created list items and marker images
remain further work, so the broader list rows remain partial.

An intervening comment now preserves the leading preformatted newline.
Zero-size preformatted tabs do not divide by zero, and zero-size glyphs emit
no text paint operators. Preserved whitespace participates in margin-collapse
rules. Advanced layout input with malformed marker text returns a diagnostic.

### Acceptance evidence

The matrix at `.quality/logs/20261006T235716Z-2440511/` passes both supported
runtimes with 100% coverage, no warnings, 1,034 ordinary tests, unchanged
performance guards, and all 90 Chromium comparisons. The required host command
also passes all 90 comparisons. `html_list_attributes.html` compares two pages
with numbering resets, reversed defaults, hidden/unmarked items, nested lists,
inside wrapping, author overrides, padded negative values, and zero-size text.

The inventory audit preserves all 17,267 unique IDs. Six attribute records moved
from planned to partial. Counts are 2,238 implemented, 4,627 partial, 2,278
planned, 421 printable exclusions, and 7,703 preserved non-printing records.
No broader feature was marked implemented.

### Performance evidence

Batch 4 reports in `.quality/performance/results/1.1.0-batch-4/reports.json` retain the
same 19 workloads, paired captures in opposite orders, pinned repeated runs,
and separate list scaling measurements. The comparison includes the fixed
1.0.0 library and the preceding optimized batch 3 snapshot. Input hashes match.
Performance acceptance remains pending while a repeatable Elixir 1.19
requisition timing difference is investigated. Passing existing guards does
not resolve that requirement.

At 1,000 styled list items, the separate feature workload produces 222,675
PDF bytes. Median reductions are about 35.3 million on Elixir 1.19 and
33.5 million on Elixir 1.20. The reports retain cold-VM labeling, raw samples,
options, hashes, and separate sampled-memory runs. Sampling excludes native
allocations and can miss brief peaks.

## Batch 5: metadata and solid text decorations

HTML head metadata supplies PDF authors, description/subject, and combined,
deduplicated keywords. Caller metadata retains precedence and the existing
PDF validator. Titles collapse ASCII whitespace while preserving non-breaking
spaces. UTF-8 charset declarations follow the input contract. Navigation,
CSP, and cookie meta directives produce located diagnostics; harmless advisory
metadata paints nothing.

`u`, `s`, and solid underline, overline, and line-through decorations retain
originating color and font geometry across descendants and wrapped lines.
Atomic inline and positioned descendants establish separate decoration scopes.
CSS supports selected-font, automatic, explicit, percentage, and font-relative
thickness. Minimum strokes and automatic placement follow pinned Chromium;
continuous strokes are allowed by CSS Text Decoration. Author clearing,
transparent color, relative movement, and zero-size glyphs have focused tests.
Further decoration styles, offsets, and broader semantic elements remain work.

TrueType parsing and CSS color validation now live in validators. Shared CSS
token splitting avoids duplicate parsing helpers. Computed decoration records
and their configurable layer limit use centralized validation; malformed
advanced layout input returns diagnostics. Ordinary text bypasses decoration
processing.

### Acceptance evidence

The final matrix at `.quality/logs/20261007T002748Z-2543765/` passes both
runtimes with 100% coverage, no warnings, 1,047 ordinary tests, and all 91
Chromium comparisons. The exact required host parity command also passes all
91 comparisons. The two-page decoration fixture keeps the original strict
per-page thresholds. Public tests check metadata extraction, precedence,
malformed metadata, located policy diagnostics, and decoration limits.

Related source records include `element:u`, `element:s`, their element sections,
and author, description, and keyword metadata definitions. Four element/section
records moved from planned to partial. The audit retains 17,267 unique IDs:
2,238 implemented, 4,631 partial, 2,274 planned, 421 printable exclusions, and
7,703 preserved non-printing records. Broad features remain partial.

### Performance evidence

Batch 5 reports in `.quality/performance/results/1.1.0-batch-5/reports.json` retain
paired measurements of all 19 workloads with matching inputs and unchanged
guards. Output sizes match the preceding batch. Opposite-order pinned runs put
the Elixir 1.19 candidate requisition at 66.431 and 66.286 ms, alongside fixed
baseline medians of 66.447 and 66.501 ms. The preceding snapshot takes 74.180
and 70.289 ms. The smaller Elixir 1.20 difference and table timing variation
still need investigation before release acceptance.

Separate scaling captures check decorated paragraphs and metadata at 1, 100,
and 1,000 items, plus nesting through 30 layers. At 1,000 decorated paragraphs,
PDF size is 234,960 bytes; median reductions are about 43.4 million on 1.19
and 40.7 million on 1.20. Raw samples, source hashes, options, content checks,
and sampled-memory runs are retained. This evidence does not complete MathML,
SVG, document composition, or the broader HTML inventory.

## Batch 6: supplementary fonts and MATH data

TrueType format-12 character maps retain supplementary Unicode mappings and
validate groups, glyph counts, and decoded work in the font validator. PDF CIDs
remain 16-bit format values, while their Unicode mappings include supplementary
characters. Subset and full-font output preserve the original extracted text.
Pinned, unmodified STIX Two Math v2.13b171 loads only when selected. Its complete
license and source hashes accompany the font; caller faces retain precedence.

Text measurement now retains decoded width prefixes within a render, avoiding
repeated scans to reach high glyph IDs. Each measurement still consumes the
existing text-work budget. Cache byte and entry limits remain enforced, and
changed width contents under the same identifier produce fresh measurements.
The 1,000-item supplementary-font workload now completes within the original
limits and retains its final text.

`Font.math_metrics/1` supplies the selected font's OpenType MATH constants,
italic corrections, accent attachments, extended shapes, corner kerning, device
adjustments, variants, and assemblies. Validators own offset, count, coverage,
glyph, and ordering checks. The shared `max_font_math_work` budget bounds decoded
bytes and expanded entries cumulatively. Font-budget memoization retains source
bytes and detects changed contents. Ordinary font loading does not decode MATH.

### Acceptance evidence

The matrix at `.quality/logs/20261007T035914Z-3018360/` passes Elixir 1.19 and
1.20 with 1,065 ordinary tests, 100% coverage, no warnings, unchanged performance
guards, and all 92 Chromium comparisons. The required host parity command also
passes all 92 comparisons. `font_supplementary_unicode.html` compares two pages
using identical pinned STIX bytes, with changed pixels below 0.22% per page.

Focused tests exercise selected/caller fonts, changed source contents, all fixed
constants, signed device adjustments, variation indices, coverage formats,
corner kerning, variants, assemblies, every truncated prefix of a synthetic
table, malformed counts/offsets/glyphs, and cumulative resource diagnostics.
The pinned STIX table supplies 889 italic corrections and 118 vertical stretch
constructions. An assembly with connectors longer than its part advance remains
valid; no extra bound beyond the OpenType format is imposed.

The inventory audit still accounts for all 17,267 unique records. No HTML row
moves to implemented on the strength of font data alone. MathML namespace
integration, token semantics, equation layout, font substitutions, and PDF
painting remain unfinished in the separate MathML checklist.

### Performance evidence

Batch 6 reports in `.quality/performance/results/1.1.0-batch-6/reports.json`
retain unchanged-workload comparisons against the fixed 1.0.0 library and the
preceding batch 5 snapshot. Two rounds on each runtime measure all 19 workloads
in opposite execution orders, with matching input hashes. Candidate PDF sizes
match the previous batch on every workload.

Longer pinned comparisons use 31 samples after five warmups, two schedulers,
and the same two CPU cores. Candidate table and requisition times are generally
at or below the preceding batch, but invoice timing needs further investigation.
On Elixir 1.20, the candidate takes 55.365 and 56.335 ms, against fixed baseline
medians of 50.423 and 49.345 ms. The added 4.942 and 6.990 ms are 9.8% and 14.2%.
On Elixir 1.19, candidate medians are 39.552 and 41.402 ms, against 34.674 and
37.429 ms. Near-unchanged reductions and passing guardrails do not explain or
justify this repeatable timing cost. Release performance acceptance remains open.

Separate supplementary-font scaling covers 1, 100, and 1,000 text blocks with
extracted-content checks, raw samples, input/library hashes, and three memory
runs. At 1,000 blocks, both runtimes produce 108,261 PDF bytes. Median reductions
are 18,015,501 on Elixir 1.19 and 17,722,638 on Elixir 1.20. Median times in this
capture are 377.073 and 331.806 ms respectively. Sampled VM peaks are 99,494,984
and 96,208,992 bytes. These samples exclude native allocations and brief peaks.

Separate MATH measurements load the pinned font and verify its axis constant,
italic corrections, vertical constructions, and radical assembly on every call.
A first font load plus MATH decode takes 45.120 ms on Elixir 1.19 and 43.168 ms
on 1.20 in these captures. Decoding MATH for an already loaded font takes 1.162
and 1.285 ms. At 1,000 calls, independent font budgets take 993.514 and
1,009.659 ms; identical selected bytes within one shared budget take 1.170 and
1.126 ms, with 131,408 and 131,524 median reductions. Each shared-budget sample
includes its initial decode. Raw samples, source hashes, and three separate
sampled-memory runs remain in the report bundle. These are font-data measurements;
equation rendering has not been implemented.

## Batch 7: namespaces, glyph geometry, and checksum work

The parser reads tokens in their actual parent context. It retains MathML and
SVG namespace identities, the specified camel-case adjustments and qualified
foreign attributes, HTML integration points, foreign CDATA, and explicit
foreign fragment contexts. Ordinary HTML trees retain their existing shape.
Malformed foreign-content recovery, scripts, event attributes, and incorrect
namespace declarations return source diagnostics. SVG and MathML painting
remain unfinished and return specific renderer diagnostics. Stylesheet
preparation now preserves these diagnostics instead of replacing them with a
stylesheet-file message.

`Font.glyph_bounds/1` returns selected-font ink bounds indexed by glyph ID.
Location tables and glyph headers are checked in validators. Source bytes and
decoded work use the existing font budgets; identical selected bytes reuse
geometry within one budget. `Font.math_metrics/1` also reads `ssty`, `flac`, and
`dtls` from the default math language system's GSUB features. Lookup order and
first-matching-subtable semantics are retained. Single, alternate, and extension
substitutions are checked against the selected font's glyph count. Unavailable
optional features leave glyphs unchanged; unsupported contextual lookups,
filtering, and feature variations return diagnostics. Layout still needs to
apply these substitutions.

The TrueType checksum loop reads words in groups and applies the 32-bit modulo
at the end. It avoids an intermediate padded copy and repeated arithmetic
masking. Independent checksum tests cover every trailing-byte length and the
whole-font checksum invariant. Controlled captures verify byte-identical PDFs.

### Acceptance evidence

The matrix at `.quality/logs/20261007T043021Z-3100019/` passes both runtimes
with 1,079 ordinary tests, 100% coverage, no warnings, performance guards,
Dialyzer, and all 92 Chromium comparisons. The required host parity command also
passes all 92. The subsequent matrix at `.quality/logs/20261007T045011Z-3167224/`
also passes both runtimes with 1,083 tests, 100% coverage, no warnings,
performance guards, Dialyzer, and all 92 parity comparisons. It includes GSUB
and the ordinary-HTML optimizations. Parser-only changes add no painted
browser fixture. Existing test modules contain the new cases.

### Performance evidence

The batch's captures live under ignored
`.quality/performance/results/1.1.0-batch-7/reports.json`. Its 46 captures retain input/library
hashes, runtime settings, raw samples, reductions, output sizes, and sampled
memory. All 19 unchanged workloads have matching input hashes, and candidate
PDF sizes match the preceding implementation. The initial whole-harness
captures contain timing disagreements, including a table slowdown, so passing
guardrails do not close release performance acceptance.

Longer checksum comparisons use five warmups, 31 samples, two schedulers,
CPUs 4 and 5, and opposite execution orders. On Elixir 1.19, invoice medians
fall from 57.156/56.517 ms before the checksum change to 41.349/40.154 ms.
On Elixir 1.20 they fall from 40.526/39.371 ms to 29.187/28.933 ms.
Every before/after PDF hash matches. These captures mitigate the earlier invoice
cost; they do not prove the cause of the batch 6 timing disagreement.

Longer namespace comparisons on the 160-table input retain identical PDF hashes.
Candidate medians are 59.304/58.760 ms on 1.19 and 57.478/59.073 ms on 1.20.
The same implementation before namespace parsing takes 59.209/60.285 ms and
59.466/59.435 ms respectively. These isolated runs do not reproduce the earlier
whole-harness timing cost. They do show approximately 1% additional reductions,
and subsequent ordinary-HTML optimizations require fresh measurements.

Selected MATH data plus ink geometry are measured separately at 1, 100, and
1,000 calls, with content checks on every call and three sampled-memory runs.
At 1,000 calls, independent budgets take 4,263.351 ms on 1.19 and 5,085.035 ms
on 1.20. One shared budget takes 12.497 and 12.625 ms, including its first decode.
These captures precede GSUB parsing and measure font data, not equation rendering.

Subsequent GSUB-plus-geometry captures verify both STIX script forms on every
call. First font loading and parsing take 44.609 ms on 1.19 and 44.573 ms on
1.20. At 1,000 independent calls, medians are 6,947.797 and 6,781.010 ms;
one shared font budget takes 13.057 and 12.889 ms. These separate captures
retain the extra selected-feature cost and include the first decode in shared
samples. They do not add work to ordinary documents.

## Batch 8: explicit selected glyph painting

The low-level PDF writer accepts `:glyph_ids` on embedded-font text boxes. Each
Unicode scalar has one selected glyph, and distinct forms receive distinct CIDs.
Normal and explicit default forms reuse CIDs where extraction agrees. Explicit
runs bypass Latin ligature substitution and automatic kerning, use selected
glyph widths, and retain the original Unicode, including supplementary
characters and literal presentation ligatures. Subset and full-font output
preserve these mappings.

Writer validation owns input shape, selected outline and width availability,
cumulative font work, conflicting font identities, and the fixed 65,535 CID
bound. No glyph geometry is decoded for ordinary text. Font-budget failures
retain the public writer operation. Empty text paints no operator, avoiding an
empty Unicode mapping being interpreted as an unsupported encoding.

### Print comparison

`font_glyph_variants.html` exercises normal, script, and scriptscript forms.
Chromium 152.0.7977.82 ignored the tested `ssty` settings on ordinary text and
MathML. The comparison therefore uses equivalent PUA aliases in a 7,576-byte
reference subset independently generated with fontTools 4.60.1. Native writing
and Chromium use identical reference bytes. The generator checks the nine
selected glyphs' contours, instructions, advances, and global hint tables against
unmodified pinned STIX. The reference family is renamed and carries its OFL;
the bundled face remains unchanged. Direct writer tests use original STIX too.
The reproducible generator is `scripts/generate-math-glyph-reference.py`.

The fixture checks selected IDs and original extracted Unicode, as well as
pixels. The host comparison changes 0.786% of pixels with the original 1% gate.
A font-size raster boundary is resolved by one unit of PDF number precision on
explicit runs that request CSS snapping. `TJ` adjustments retain the measured
advances; focused tests check this compensation. Unsnapped runs retain their
specified size. These are writer checks and do not establish MathML layout
compatibility.

The matrix at `.quality/logs/20261007T052657Z-3275629/` passes on both runtimes
with 1,087 ordinary tests, 100% coverage, no warnings, unchanged performance
guards, Dialyzer, and 93 parity comparisons. The required host command also
passes all 93 comparisons.

### Performance evidence

`.quality/performance/results/1.1.0-batch-8/reports.json` retains 14 captures.
Two serialized rounds on each runtime compare the fixed 1.0.0 baseline,
batch 7, and this batch on CPU cores 4 and 5 with two BEAM schedulers. All input
hashes match and all candidate output sizes match batch 7. Most reduction
changes are below 0.4%; the 4,000-box writer workload adds 1.55% and 2.52%
reductions on 1.19 and 1.20. Its median timing rises 2.07% and 0.72%.
The 160-table workload remains 14.09% and 11.92% slower than the fixed baseline
in these whole-suite runs, while isolated earlier captures disagreed. This
unresolved measurement still blocks release performance acceptance.

Separate glyph-painting captures measure 1, 100, and 1,000 selected STIX glyphs,
including source loading, first painting, warm samples, reductions, sampled VM
memory, and PDF size. The 1,000-glyph input uses two pages, keeps every glyph
within page bounds, and verifies every original character after extraction.
Median painting costs are 18.386 ms and 15.948 ms; font loading and substitution
parsing cost 42.473 ms and 42.260 ms. Each input produces identical PDF hashes
across all samples and both runtimes. Sampled VM memory excludes native
allocations and brief peaks. These feature captures use 12 schedulers and are
separate from the controlled unchanged-workload comparisons.

## Batch 9: mathematical glyph and row measurement

`MathLayout.glyph/4` measures one selected mathematical glyph and keeps its
original Unicode. Validator-owned input preparation applies all 112 MathML
Core italic mappings, selects script or scriptscript forms, checks source
geometry and advances, and resolves device adjustments in CSS pixels. Missing
glyphs and unresolved variation adjustments return actionable diagnostics.
The selected face supplies every metric. No fallback face supplies MATH data.

`MathLayout.row/1` positions measured children on a shared alphabetic baseline,
adds preceding italic correction before upright children and at the row end,
and preserves signed ink extents. It handles empty rows and measured nesting.
These functions provide geometry for later token and equation composition;
they do not establish complete MathML token or row rendering.

`MathLayout.with_context/1` shares font decoding and the existing advance cache.
Glyph advances use indexed tuples instead of scanning a font's width list for
every glyph. Shared font and layout work limits bound decoding and row work.
Cache reuse checks the selected font term and bytes; changed widths or source
bytes cannot reuse old geometry. Ordinary HTML does not invoke these functions.

GSUB preparation now retains a parallel `:substitution_kinds` map. A single
replacement applies at either enabled script level; an alternate lookup selects
the requested alternate. Extension subtables must share a lookup type. Existing
`:substitutions` maps retain their prior shape and ordering.

### Print evidence and remaining differences

`math_glyph_rows.html` compares geometry with equivalent CSS positions calculated
independently from STIX using fontTools. A renamed 8,032-byte reference subset
retains selected MATH italic/accent records and checks them, contours, hints,
and advances against the unmodified source. Both painting paths use the same
reference bytes. The 30.75-point comparison changes 0.802% of pixels at the
unchanged 1% threshold and verifies all original characters after extraction.
The existing 30-point writer comparison remains unchanged.

The direct 30-point MathML comparison is retained under
`.quality/math-row-unadjusted/`. Chromium's raster ink bounds shift baseline
positions from the scaled outline bounds by 0.54, 0.18, and 0.75 points. That
comparison changes 3.640% of pixels. A 30-point equivalent CSS experiment also
changed 1.020% near raster boundaries. These unresolved differences must be
addressed during renderer integration. No MathML inventory row is complete.

### Acceptance evidence

The matrix at `.quality/logs/20261007T090320Z-3592026/` passes on Elixir 1.19 and
1.20 with 1,094 ordinary tests, 100% coverage, no warnings, original performance
guards, Dialyzer, and 94 parity comparisons. Focused tests cover exceptional
Unicode mappings, changed source data and widths, script lookup kinds and
order, unsupported inputs, font/layout budgets, device values, empty geometry,
signed ink extents, and row offsets. The required host command passes all
94 parity comparisons. The inventory audit still accounts for all 17,267 IDs.

### Performance evidence

`.quality/performance/results/1.1.0-batch-9/reports.json` retains 26 captures.
The two serialized whole-suite rounds per runtime preserve identical input
hashes and unchanged output sizes relative to batch 8. Most reduction changes
are below 0.3%. The 1.20 100-row workload's whole-suite median varied by 28.90%
against the preceding build, with a 0.93% reduction change. Longer isolated
31-sample runs measured candidate medians of 144.258/145.530 ms against
142.687/140.843 ms, a 2.21% aggregate difference. On 1.19 the equivalent
medians are 158.610/153.908 ms against 155.371/156.345 ms, a 0.26% difference.
All previous/candidate isolated PDFs are byte-identical. These measurements do
not establish release performance acceptance. The earlier table slowdown and
whole-suite variation remain open; no baseline or threshold changed.

A separate feature benchmark measures selected glyphs, three script levels,
row placement, and PDF painting for 1, 100, and 1,000 three-character equations.
Each document has its own shared decoding and advance caches. The largest
input has 3,000 glyphs across four pages, verifies every original character,
and keeps all glyphs inside page bounds. Median measurement/painting costs
are 46.995 ms and 36.892 ms, with about 4.906 and 3.632 million reductions.
The resulting 107,056-byte PDFs have identical hashes across all samples and
both runtimes. First font loading costs 36.797 ms and 40.673 ms separately.
Sampled VM memory excludes native allocations and brief peaks. Feature captures
use 12 schedulers; unchanged-workload comparisons use CPUs 4 and 5 with two.
HTML token styling, equation operators, and combined document layout are outside
this geometry benchmark.

## Remaining milestone work

Continue complete static HTML and defaults, mixed/nested content, control value
semantics, composition, stylesheet/media and metadata/link behavior, image
selection/maps, namespace integration, inline SVG, controlled embedded documents,
and MathML. Remaining parser work includes optional rules for newly supported
elements, complete fragment contexts, and input/document-mode details.

Each feature needs its source records, focused tests, visual comparisons,
resource-policy checks, performance measurements, and a successful matrix.
SVG and MathML need their separate standards checklists. Dependencies explicitly
scheduled for later milestones retain those labels. This milestone is not
complete and the target compatibility statement is not yet a release claim.

## Batch 10: Unicode replacement for glyph groups

The low-level writer accepts a text-only `:glyph_group` containing `:actual_text`
and drawing `:boxes`. Validation checks its UTF-8 string and every child, rejects
nested groups and caller-supplied internal markers, reserves cumulative logical
text bytes, and bounds expansion before flattening. Ordinary pages avoid the
flattening pass. The writer uses the shared PDF text-string codec and emits
`/Span` marked content around the pieces.

Text extraction prepares literal or named property dictionaries, resolves
indirect replacement values, and validates balanced scopes and nesting. The
outermost replacement emits one span. Empty strings suppress their pieces.
All physical text operands retain font, geometry, byte and span validation.
Replacement bytes count toward the shared extraction budget, including repeated
Form executions. A replacement span retains the first piece's starting context
and the last piece's endpoint, with painting and clipping flags combined.
Adjacent replacements have no inserted word break in source-order extraction.
Groups containing only images or paths and structure-tree replacement text
remain outside this text-operand implementation.

The existing math-row fixture uses groups and now asserts exact original Unicode
without stripping whitespace. Its pixel difference remains 0.802%. This is
writer and extraction evidence; native HTML/MathML layout and glyph assembly
construction remain unfinished.

Acceptance gates pass in `.quality/logs/20261007T101303Z-3683184/` on Elixir
1.19 and 1.20, with 1,106 tests, 100% coverage, 94 browser comparisons,
performance guards and Dialyzer, and no matrix warnings. The first matrix run
found a stale limit-count assertion, a canonical-runtime formatting difference
and uncovered dictionary-limit/decoding branches; all were resolved.
The exact host parity command also passes all 94 comparisons in
`.quality/batch-10-host-parity.log`. The inventory audit retains all 17,267 unique
IDs and the existing category counts. No broad MathML inventory row is complete.

### Performance evidence

All 45 captures are consolidated in
`.quality/performance/results/1.1.0-batch-10/reports.json`, with source hashes and
raw-byte backups. The initial baseline capture overlapped the host parity run;
it is retained under `overlap-aborted` and excluded from acceptance comparisons.
The first isolated extraction attempt generated a different PDF under each
renderer revision; those six `varying-input` captures are retained for diagnosis
and excluded from acceptance. The repeated controlled run uses one frozen PDF
with SHA-256 `9efd62895984339d729bda999816395acdc05d3f72985e91769b662af3835a65`.

Two full-suite rounds use CPUs 4 and 5, two BEAM schedulers, and unchanged fixed
1.0.0 and previous-batch references. All 19 input hashes match, and previous versus
candidate result sizes are unchanged. The ordinary 4,000-box writer increases
reductions by 0.18–0.23%. The full-suite purchase-order timing appeared 5.53–14.19%
slower, so it was repeated with 31 samples and five warmups in two rounds per
runtime. Its aggregate candidate versus previous timing is -1.64% on 1.19 and
+0.04% on 1.20. Every previous/candidate PDF is byte-identical in those runs.

Full-suite visual-extraction timing on 1.20 appeared 7.56–7.70% slower. The
31-sample isolated run against the frozen PDF puts aggregate timing at -1.29%
and +0.90% versus the previous batch, with +0.46% and +0.47% reductions. Extracted
text is byte-identical in every sample. The older fixed-baseline table timing
regression remains open; this batch does not reset or relax any threshold.

The separate feature benchmark measures 1, 100 and 1,000 grouped three-character
math rows. Measurement/painting and source-order extraction are timed separately.
At 1,000 groups, 3,000 characters survive exactly across four pages, with 1,000
logical spans. Font loading takes 38.430/39.798 ms. Median measurement/painting is
54.641/41.325 ms and extraction is 146.090/137.861 ms on 1.19/1.20. Each PDF is
107,757 bytes, with identical hashes across samples and runtimes. Sampled total
VM memory is retained, excluding native allocations and brief peaks. This work
measures grouped glyph painting and replacement extraction; actual stretch
assemblies and native HTML/MathML composition are still unfinished.

## Batch 11: Selected-font stretch geometry

`MathLayout.stretch/6` measures a natural glyph, selects the first sufficient
ordered variant, or builds a horizontal/vertical assembly from the selected
font. Each extender repeats equally. Connector overlap stays within the font's
minimum and actual joining ends. Original parts supply orthogonal metrics even
when an extender is omitted. Assembly italic correction includes device data.
Unusable constructions fall back to their last variant. Missing constructions,
invalid requests and exhausted budgets return diagnostics. Repetition reserves
font work and drawing-box capacity before allocating expanded lists.

The dated MathML Core assembly text conflicts with OpenType's unused outer
connectors and Chromium's actual-join checks. The implementation follows actual
joins. All-extender assemblies require a nonempty result for a positive target.
The [MathML checklist](html-to-pdf-mathml-1.1.0-checklist.md) records both decisions
and their primary sources. STIX bytes remain unchanged. Its right-arrow assembly
has a zero connector at a required join and uses its final variant.
The pinned Chromium 152 stretch-source SHA-256 is
`6cfffc6f7a9afcc1a00285fee7e88a2b9e3ccf46e0c1cb355efc5b9c0721d374`.
The decoded primary source is retained under ignored batch-11 evidence.

Focused tests use the existing font test file. They cover variant boundaries,
zero size, missing constructions, unusable connectors, zero/one/many extenders,
unused-part metrics, device/variation data, exact offsets and cumulative limits.
An independent FontTools reference uses rational arithmetic and identical renamed
subset bytes in both painting paths. The stretch fixture checks natural/variant
parentheses, vertical parentheses/radicals/integrals, a horizontal brace, selected
glyph identities and one exact Unicode string per operator.

The first comparison exceeded the pixel gate. Matching physical page sizes
resolved Chromium's 260-to-259.92-point print-page rounding. Remaining differences
came from horizontal raster ties. Embedded contours, instructions and global hint
tables were identical. Resolving explicit selected-glyph ties toward the left
with the existing fixed precision bias makes all three glyph fixtures pixel-identical.
The preserved 30-point CSS row experiment also now matches exactly, so the
accepted row fixture has returned to 30 points. Direct MathML raster-ink metrics
and HTML/MathML integration remain unfinished. No broad standards row is complete.

### Performance evidence

All 38 captures are consolidated in
`.quality/performance/results/1.1.0-batch-11/reports.json`, with raw-byte backups
and SHA-256 hashes. Two full-suite rounds use CPUs 4 and 5, two schedulers, the
fixed 1.0.0 reference and the previous batch. All 19 input hashes match and
previous/candidate output sizes are identical. Ordinary 4,000-box writing changes
reductions by at most 0.001%; measured timing changes are -1.03%/+0.93% on
1.19/1.20. Full-suite 1.20 paragraph timing appeared 5.48% slower. Repeating that
identical input with 31 samples, five warmups and two rounds puts aggregate
candidate timing at +0.55%/+0.26%, with byte-identical previous/candidate PDFs.
The older fixed-baseline 160-table timing gap remains +12.36%/+13.95% in these
whole-suite captures and remains a release blocker. No threshold or baseline
was relaxed. Large-image and 100-row whole-suite timings also vary substantially
across captures; isolated evidence is required before attributing a change.

Feature measurements cover 1, 100 and 1,000 operators. At 1,000 operators,
7,000 pieces paint across 36 pages and extract as exactly 1,000 logical spans.
Source font loading takes 40.854/37.768 ms. Median measurement/painting is
96.269/67.509 ms; extraction is 363.941/350.594 ms on 1.19/1.20.
The PDF is 139,835 bytes, identical across samples and runtimes, SHA-256
`64a50b83125fb1e78130770b9827efd808de67e7f384af91477762e8b2e381a7`.
Ten-millisecond sampled total VM memory peaks at 84,924,208/86,860,328 bytes for
that workload, excluding native allocations and brief peaks. These measurements
cover stretch construction, painting and replacement extraction; equation
composition and host HTML integration are still pending.

The final restored-30-point matrix passes in
`.quality/logs/20261007T112644Z-3889825/` on both supported runtimes, with 1,112
tests, 100% coverage, all 95 browser comparisons, performance guards and Dialyzer.
There are no warnings. Both first-run parity failures were inspected and traced
to the same 1.247% raster difference, now resolved. The exact host command also
passes all 95 comparisons in `.quality/batch-11-host-parity-restored-30.log`.
The inventory audit retains all 17,267 unique IDs and the existing categories.

The fixed-baseline table gap was also isolated with 31 samples, five warmups and
two rounds per runtime. Candidate versus fixed timing is -10.78%/-10.76%, and
versus previous is +0.54%/-1.01%. All baseline, previous and candidate table PDFs
are byte-identical. The whole-suite slowdown therefore conflicts with the long
isolated result and cannot yet be attributed to renderer code. Profiling does
identify repeated rem-value traversal on elements with no author declarations;
the next batch removes that avoidable pass and repeats the comparisons.


## Batch 12: Avoid unused style traversal

Elements with no author declarations skip recursive rem-value resolution.
Defaults contain no rem values, and inherited lengths have already been resolved.
Declaration validation, custom properties and semantic font-relative defaults
remain in the same path. A focused test in the existing style test file compares
unstyled descendants with equivalent neutral declarations that exercise the full
resolver, including inherited root lengths, headings, lists and table groups.

The final matrix passes in `.quality/logs/20261007T114226Z-3949077/` on both
runtimes, with 1,113 tests, 100% coverage, 95 browser comparisons, performance
guards and Dialyzer. It has no warnings. The earlier failed matrix caught a test
calling an unavailable parser function; both failure logs and the resulting
warning were inspected and the test corrected. The exact host parity command
passes in `.quality/batch-12-host-parity.log`. The inventory audit retains all
17,267 unique IDs and the existing categories.

All 36 performance captures are consolidated under ignored
`.quality/performance/results/1.1.0-batch-12/reports.json`, with raw-byte backups
and hashes. Full-suite input hashes match and previous/candidate output sizes
are unchanged. Two rounds of isolated 160-table runs use 31 samples and five
warmups on each runtime. Candidate median time is 60.096/58.081 ms on 1.19/1.20,
2.96%/4.32% faster than the previous batch and 29.88%/14.10% faster than the fixed
baseline. Reductions fall 14.38%/13.57% against the previous batch. Every table
PDF, including the baseline, is byte-identical. Cold-render measurements are
retained, but two samples do not establish a stable cold timing difference.
Whole-suite table timing still conflicts with the longer isolated measurements.

Large-PNG timing required its own isolated comparison. The same ten-image input
and asset manifest, 31 samples, five warmups and two rounds produce candidate
medians of 482.055/476.709 ms. That is 0.50%/0.22% slower than the previous batch,
with reductions down 0.05%/0.07%. Against the fixed baseline, however, time is
9.05%/9.64% slower and reductions rise 0.58%/0.53%. All image PDFs are
byte-identical across revisions. Cold and warmed captures agree that a
fixed-baseline large-image timing gap remains. Its cause is unproven and it
blocks release. No baseline, fixture, tolerance or threshold was relaxed.

Ordinary 4,000-box writing changes reductions by at most 0.001%; timing is
+1.17%/+0.52% versus the previous batch. Full-suite 100-row timings vary widely
between rounds and runtimes; they do not establish a new attributable regression.
The complete reports retain elapsed time, reductions, sampled memory, output
size, cold results and controlled environment details.


## Batch 13: Bound extreme math repetition

The math validator checks repetition growth against `max_font_math_work` before
dividing. A valid bounded target with a font size of `1.0e-308` now returns a
shared font resource diagnostic with operation `measure_math`; it previously
raised `ArithmeticError`. Expansion still reserves its actual cumulative work
and drawing capacity before allocation. Single-glyph validation matches one
UTF-8 scalar directly, avoiding a full character-list allocation for long invalid
input. Existing font tests cover both failures. Ordinary glyph placement remains
unchanged.

The matrix passes in `.quality/logs/20261007T121651Z-4020544/` on both supported
runtimes, with 1,113 tests, 100% coverage, all 95 browser comparisons, performance
guards and Dialyzer. There are no warnings. The exact host command passes in
`.quality/batch-13-host-parity.log`. The inventory audit retains all 17,267 unique
IDs and the existing categories.

Two full-suite rounds retain the fixed baseline and preceding batch on both
runtimes, with CPUs 4 and 5 and two schedulers. All 19 input hashes match and
previous/candidate output sizes are unchanged. Ordinary 4,000-box writing changes
reductions by +0.0013%/+0.0010%; measured timing changes by +2.15%/-0.66% on
1.19/1.20. Invoice, report and form timing differences remain within 3.55% of the
previous batch. Large-PNG timing is +1.13%/+2.73% versus previous and
+8.62%/+13.87% versus the fixed baseline, retaining the known release blocker.
Whole-suite table timing is +4.90%/+8.31% versus fixed; the contradictory longer
isolated results from batch 12 remain preserved. No threshold or baseline changes.

The separate stretch benchmark repeats 1, 100 and 1,000 operators, including exact
Unicode extraction and sampled memory. At 1,000 operators, median measurement and painting takes 90.430/63.783 ms;
extraction takes 313.827/312.902 ms on 1.19/1.20. Font loading takes
45.026/39.208 ms. All PDFs are 139,835 bytes with SHA-256
`64a50b83125fb1e78130770b9827efd808de67e7f384af91477762e8b2e381a7`.
Sampled VM peaks are 80,989,216/84,154,248 bytes, excluding native allocations
and brief peaks. Its reports and the full-suite captures are retained with
raw-byte backups under ignored `.quality/`.
A benchmark snapshot initially omitted its dependency lock and used host-only
symlink targets. Setup stopped before capturing results. The snapshot now uses
the unchanged lock and relative links that work in Docker; the failure logs are
retained separately. No captured result was overwritten.

A diagnostic profile of ten repeated large-PNG renders records 33,505,280 calls
to history-window updates. Those updates and their list concatenations account
for about 60% of profiled candidate time. Baseline and candidate decoder call
counts are identical; tracing does not reproduce the unprofiled slowdown and
does not establish its cause. The next experiment replaces history-list rebuilding
with a fixed-size byte-history tuple and direct access to the previous row.


The isolated decoder experiment uses 31 samples, five warmups and two rounds per
runtime. All input hashes, asset manifests, cold PDF hashes and 31-sample PDF
hashes match across the fixed baseline, accepted batch-13 candidate and prototype.
The PDF SHA-256 is
`a544a01156be1f3beb52226aca4c2d0669b11bf3a8c40583ff391769f1a3d42a`.
Prototype medians are 236.536/265.730 ms on 1.19/1.20, 50.44%/44.17% faster than
the accepted candidate and 45.82%/37.94% faster than the fixed baseline.
Reductions fall 34.32%/34.08% against the candidate. Cold time falls
45.86%/45.30%. These are experimental results; the following batch applies the
change and requires its own matrix and final performance comparisons.
All 26 captures are consolidated in
`.quality/performance/results/1.1.0-batch-13/reports.json`. Fourteen capture the
accepted math-bound change; twelve are explicitly named decoder experiments.
The source hashes distinguish those revisions. Raw-byte backups remain ignored.


## Batch 14: Avoid per-byte PNG history lists

PNG reconstruction retains the decoded byte at each position within a pixel in a
small fixed-size tuple. It reads the upper-left byte directly from the complete
previous row, which the decoder already retains. This replaces two queue updates
and their list concatenations per byte. The tuple size follows PNG sample depth
and channel count; it introduces no tunable ceiling. Validation, decoding budgets,
filters, alpha samples, interlace passes and PDF painting remain unchanged.

The [PNG Recommendation's filter rules](https://www.w3.org/TR/2025/REC-png-3-20250624/#9Filters)
require reconstructed left, upper and upper-left bytes, with zero boundary values.
The fixed-size tuple starts with zeroes for the left edge, while the retained prior
row supplies upper-left bytes after the first pixel. Existing decoder tests check
exact samples for every supported color type and bit depth, all five filters,
Adam7 and non-interlaced input, narrow images, transparency and exhausted budgets.
All nine focused decoder/validator tests pass. No new test file or visual behavior
is introduced; the existing PNG browser fixtures remain the print acceptance cases.

The final matrix passes in `.quality/logs/20261007T232333Z-231369/` on both
supported runtimes, with 1,113 tests, 100% coverage, all 95 browser comparisons,
performance guards and Dialyzer. There are no warnings. The exact host command
passes in `.quality/batch-14-host-parity.log`; the inventory audit retains all
17,267 unique IDs and the existing categories.

All 38 performance captures are consolidated in
`.quality/performance/results/1.1.0-batch-14/reports.json`, with raw-byte backups
and source hashes. Twelve cover the unchanged 19-workload suite, twelve isolate
large images, twelve isolate tables and two alternate decoder versions within
one VM. Inputs and previous/candidate output sizes match throughout the suite.
Isolated image and table PDFs, including fixed-baseline outputs and cold renders,
are byte-identical across every sample. No limits or thresholds change.

The final large-PNG comparisons use 31 samples, five warmups and two rounds on
each runtime. Candidate medians are 254.496/318.871 ms on 1.19/1.20,
57.60%/48.83% faster than the previous batch and 43.65%/48.05% faster than the
fixed baseline. Reductions fall 34.93%/34.79% against previous, with cold time
down 41.97%/43.92%. Both the prototype and production comparisons remove the
confirmed large-PNG baseline gap while preserving exact PDF output.

Whole-suite timings swing sharply, including a 63.72% change in the unchanged
PDF-only writer on 1.20 and large extractor changes. Those captures remain
preserved. The diagnostic comparison alternates old/new decoder BEAM files inside
one VM for 31 samples per revision, with order reversed on every iteration.
Only decoder source differs; every other library module is shared. Module loading
and collection occur outside timing, and every output hash matches. Relative
candidate timings on 1.19/1.20 are -1.69%/-0.76% for 100-row rendering,
-0.41%/-1.21% for tables, -0.90%/0.00% for the 4,000-box writer and
+1.28%/+0.30% for default-layout extraction of a frozen 100-row PDF.
Invoices improve 12.57%/4.45% with fewer decoder reductions. The large
whole-suite changes do not reproduce in these alternating comparisons.

Separate 31-sample table runs retain the fixed baseline. Candidate medians are
57.423/55.535 ms, +0.87%/+0.08% versus previous and -29.17%/-13.24% versus fixed.
Reductions change -0.12%/+0.02% against previous. Cold table times are
133.037/132.720 ms, +2.53%/-0.64% versus previous and +1.51%/+4.58% versus fixed.
Only two cold samples per revision were taken, so this does not establish cold
performance acceptance. The conflicting whole-suite evidence and cold-render
uncertainty remain recorded for the final release review. Sampled VM memory and
output sizes remain in the complete suite captures.

## Batch 15: Hidden input rendering

The parser accepts case-insensitive `type=hidden` with the existing supported
`name`, `value`, `disabled`, and common attributes. Parsed values remain available
to selectors. `HtmlValidator` owns the shared input-type list and applicability
checks; `checked` remains invalid on hidden inputs. Unsupported attributes,
executable attributes, and invalid CSS still return source diagnostics.

The pinned 2026-10-05 HTML snapshot's hidden-input state and UA rendering rule
supply the behavior. Related source IDs are `review-source-1442` through
`review-source-1445` and `review-source-5773`. The UA `display: none !important`
rule overrides author display declarations, including inline important values.
Style finalization applies this rule after CSS validation, before background
asset resolution, generated content, and PDF control preparation. Hidden inputs
produce no artwork, layout spacing, widget, or AcroForm field in either form mode.
Their names do not reserve PDF field names. Font selection still follows the
existing style pipeline; this batch does not claim to skip author font loading.

Testing exposed an existing bug that collected absolutely positioned elements
with `display: none` into positioned layout. The collector now retains hidden
nodes in ordinary flow, where the existing no-layout rule handles them, without
collecting their positioned descendants. Focused layout tests cover hidden inputs
and ordinary hidden spans containing positioned descendants.

The new `html_hidden_inputs.html` fixture retains the existing pixel thresholds.
It covers case variants, inline and table-cell placement, empty/disabled controls,
positioning, generated content, and author important display overrides. The first
host comparison exposed an unrelated automatic table-width difference. The fixture
now specifies its table width to isolate hidden-control behavior; no acceptance
threshold changed. Public tests compare byte-identical output with equivalent
empty inline boundaries, blank-page output, extracted spans, and field inspection.
The existing extractor's word-separation heuristic remains unchanged.

Broader input types, attribute coverage, form ownership, sanitization, constraints,
submission exclusions, and composition remain attached to the partial input row.
No broad inventory row is marked complete. The audit retains all 17,267 unique IDs
and its category accounting. Acceptance and performance results follow below.

The final quality matrix passes in `.quality/logs/20261007T235414Z-357261/`.
Both runtimes pass 1,117 tests, 100% coverage, all 96 browser comparisons,
performance guards and Dialyzer, with no warnings. The exact required host
command passes in `.quality/batch-15-host-parity-final.log`. The hidden-input
fixture changes 0.297% of pixels on the host at the existing threshold.

All 38 captures are consolidated in
`.quality/performance/results/1.1.0-batch-15/reports.json`. Twelve compare the
unchanged suite, 24 use fresh VMs for hidden-input scaling and two alternate
previous/candidate HtmlValidator, Style and Layout modules inside one VM.
Every ordinary input hash and previous/candidate output size matches.
Whole-suite previous/candidate reductions differ by less than 0.13%.
Most timings stay within 5%; the 1.19 PNG capture is 9.61% slower. A 31-sample
alternating comparison does not reproduce that gap: PNG time changes
+0.84%/-0.38% on 1.19/1.20, with byte-identical output. Only the three batch-15
modules differ in that experiment; decoder bytes remain the accepted batch-14
version. No baseline or threshold changes. Fixed-baseline 100-row and table
timings still conflict with earlier isolated evidence and remain part of the
unresolved final performance review.

Scaling uses zero, one, 100 and 1,000 controls, three fresh VMs per size/runtime,
five warmups and 31 repeated samples per VM. Every cold and repeated PDF matches
the zero-control reference byte for byte: 77,663 bytes with SHA-256
`64cff32db76c2455dbb65165150777afcb447f2825ed9ff0423e27e1145a5ca4`.
At 1,000 controls, warm medians are 50.750/50.700 ms and first-render medians
117.465/114.373 ms on 1.19/1.20. Added parsing/style work versus zero controls
is 41.988/42.086 ms and about 4.65/4.57 million reductions. Sampled VM peaks
are 84,965,120/83,562,544 bytes, excluding native allocations and brief peaks.
A failed JSON options encoding attempt produced no report; its log remains
retained separately, and the corrected writer records options as a map.

The exact UA rule `ua-css:7` moves from partial to implemented after acceptance.
The broader input element remains partial. Its related hidden-state records are
`section:hidden-state-(type=hidden)`, `definition:review-source-1442`,
`algorithm:702`, `requirement:609` and `requirement:610`. Their non-printing
classification still excludes painting/submission; their support notes now reflect
inert acceptance. The final audit has 2,239 implemented, 4,630 partial, 2,274 planned
and 421 print-related excluded records, plus 7,703 preserved non-printing records.
All 17,267 IDs remain unique and accounted for.

## Batch 16: Initial text-input values

`HtmlValidator.prepare_input_attributes/1` strips CR and LF from the validated
initial value of text inputs, including an omitted type and case variants. Style
calls it after selectors have used the authored attributes, then passes one
prepared map to static content and interactive field preparation. The parsed tree
retains the authored value. Tabs, form feeds, spaces and Unicode separators remain
in the control value. Checkbox/radio/hidden values and textarea line breaks stay
unchanged. No new input type or failure contract is introduced.

The pinned 2026-10-05 HTML snapshot's `review-source-1451`, inventory
`algorithm:704`, names this value-sanitization algorithm for text and search.
Search appearance is still unsupported, so the shared row remains partial. The
broader text-input attributes, constraints, ownership and appearance also remain
partial. The new helper lives in the validator; consumers do not sanitize again.

The new empty value exposed an existing intrinsic-height bug. Shared inline
height measurement now retains at least one line for a text control, including
empty runs. Explicit CSS sizes still resolve through the existing size rules.
Tests compare empty, nonempty and newline-only controls in block, flex, grid and
inline-block placement. Public tests compare exact PDF bytes and initial field
values in both form modes, including all-newline values. Selector tests distinguish
a raw CRLF attribute from an authored `AB` attribute even though both initial
control values become `AB`.

`html_input_values.html` covers CR/LF/CRLF references, default/uppercase types,
original-value selectors, empty state, table placement, disabled controls and an
authored preformatted style. The first comparison exposed the empty-height bug.
After that fix, fractional control painting still differs from Chromium at 10pt
with 13pt line height and 2pt padding. Its source, PDF/raster artifacts and failure
measurements remain in `.quality/batch-16-fractional-controls/`. The focused
value fixture uses integer CSS-pixel font/line/padding settings; its host changed
ratio is 0.279% at the original threshold. This does not establish complete
fractional widget painting. That existing appearance gap remains attached to the
broader input work. Acceptance and performance evidence follow below.


The final quality matrix passes in `.quality/logs/20261008T001618Z-461971/`.
Both supported runtimes pass 1,121 tests, 100% coverage, all 97 browser
comparisons, performance guards and Dialyzer, with no warnings. The exact host
parity command passes in `.quality/batch-16-host-parity-final.log`.

All 32 captures are consolidated in
`.quality/performance/results/1.1.0-batch-16/reports.json`. Twelve compare the
unchanged suite against the fixed baseline and batch 15, fourteen measure input
values, two alternate the three changed modules inside one VM, and four repeat
the paragraph investigation. All 19 ordinary input hashes and previous/candidate
output sizes match. Whole-suite timing outliers include a 13.45% purchase-order
increase on 1.19 and an 8.01% table increase on 1.20. Alternating comparisons
reduce these differences to -0.48% and +1.83%, with identical output hashes.
An initial alternating paragraph run is 29.12% slower on 1.19. Two further fresh
VMs per runtime, each with five warmups and 61 samples per revision, give
+0.72%/-1.41% on 1.19 and -0.40%/-1.80% on 1.20. Reductions differ by less than
0.14% in these repeats, and all PDFs match byte for byte. The outlier remains
retained. It does not reproduce as a consistent batch-16 source regression.
Earlier fixed-baseline whole-suite and cold-render uncertainty remains open
for the release review.

Feature captures alternate raw newline character references with their literal
sanitized equivalents. Each successful size/mode/runtime capture uses a fresh
VM, five warmups and 31 samples per input. Every first and repeated PDF is
byte-identical to its equivalent, with the expected logical text or field values.
At 1,000 static controls, raw-value warm medians are 102.439/81.973 ms on
1.19/1.20. First-render times are 157.472/152.623 ms, and sampled VM peaks are
85,197,184/83,517,472 bytes. At 500 interactive controls, warm medians are
280.754/261.023 ms, first-render times 350.829/315.697 ms, sampled VM peaks
117,777,112/103,068,208 bytes, and output size 928,382 bytes. Each size has only
one first-render sample per runtime, so these are exploratory cold measurements.
VM sampling excludes native allocations and brief peaks. Differences between
raw and literal inputs include character-reference parsing as well as sanitation.

Both 1,000-control interactive captures fail with the existing per-render
`max_rendered_pages` limit of 1,000. Each control creates two appearance pages,
so the appearance writer receives 2,000 pages at once. The writer checks that
render separately from the main document. The captured scope string originally
called this cumulative; that description was incorrect. The retained raw reports
and their hashes remain unchanged. Cross-document cumulative page budgeting is
still unfinished. Raw and sanitized inputs return the same actionable writer
resource diagnostic. These two captures record capacity failures, not PDFs.
The limits and content remain unchanged. A failed span-count assumption and
the first capacity attempt remain in separate diagnostic logs; no successful
capture was overwritten. The inventory audit retains the batch-15 counts and
all 17,267 unique IDs. The shared text/search sanitation row remains partial
because search controls are still unsupported.


## Batch 17: Initial search-input state

The shared validator accepts case-insensitive `type=search` with the existing
supported name, value, disabled and common attributes. Search and text use one
CR/LF sanitation rule after selectors have matched authored attributes. Search
controls use text-control layout, including a line of intrinsic height when
empty, and create PDF text fields with the existing generated names and disabled
flags. No new execution, submission or search action is introduced. Checked,
readonly, pattern and executable attributes still return HTML diagnostics.

Focused parser, validator, style, layout and public tests extend the existing
files. Public tests compare byte-identical search/text PDFs in both form modes,
inspect values and flags, and cover missing names and empty sanitized values.
The new `html_search_inputs.html` fixture compares author-styled case variants,
newlines, original attribute selectors, missing/disabled values and table cells.
It retains the original pixel thresholds. Native widget theme support, remaining
attributes, constraints, form ownership and activation remain attached to the
partial input/state rows.

Before this batch, the fractional-control investigation tested static output,
snapped widget bounds and filled-border prototypes. Static output passes at
0.857% changed pixels. Snapped bounds align extracted text coordinates with
static output, but the interactive comparison still changes 2.552% of pixels.
Filled-border painting changes 6.046%. These prototypes remain under `.quality`
and were not added to the renderer. The fractional appearance gap remains open.
The fixture also exposes the existing exact-comparison bug for HTML type
attribute selectors. Chromium applies `[type="SEARCH"]` to lowercase search
values too. The native matcher currently does not. Its pixel threshold passes,
but complete attribute-selector casing remains open and is not claimed by this
value-state batch. Acceptance and performance evidence follow below.


The final quality matrix passes in `.quality/logs/20261008T004237Z-520685/`.
Both runtimes pass 1,123 tests, 100% coverage, all 98 browser comparisons,
performance guards and Dialyzer, with no warnings. The exact host command passes
in `.quality/batch-17-host-parity.log`. The search fixture changes 0.747% of host
pixels at the existing threshold, with the casing disagreement recorded above.

All 30 captures are consolidated in
`.quality/performance/results/1.1.0-batch-17/reports.json`. Twelve retain fixed
and previous ordinary-suite comparisons, twelve measure search scaling, two
alternate the three changed modules and four investigate table/trim timing.
All 19 ordinary input hashes and previous/candidate output sizes match.
The initial alternating table comparison is 11.34% slower on 1.19. Two additional
fresh VMs per runtime, each with five warmups and 61 samples per revision, give
+0.57%/-1.11% on 1.19 and +0.73%/-1.07% on 1.20. Trim-card repeat timings stay
within 1.32%. Every alternating PDF hash matches. The large table and trim
outliers do not reproduce consistently. The original measurements remain retained;
fixed-baseline whole-suite and exploratory cold uncertainty remains open for the
release review. A summarizer initially expected the consolidated bundle too early;
its log remains retained, and reporting resumed without overwriting captures.

Search scaling uses fresh VMs, five warmups and 31 samples per raw/literal input.
Every first and repeated PDF matches the sanitized search and equivalent text
control exactly, with expected static text or interactive field values. At 1,000
static controls, raw-value warm medians are 89.393/82.352 ms on 1.19/1.20,
first-render times 160.832/176.177 ms, sampled VM peaks 85,232,680/83,571,584 bytes
and output size 94,887 bytes. At 500 interactive controls, warm medians are
269.293/265.130 ms, first-render times 339.867/320.289 ms, sampled VM peaks
121,917,176/103,123,032 bytes and output size 928,382 bytes. One first-render
sample per size/mode/runtime is exploratory; sampled VM memory excludes native
allocations and brief peaks. Limits, baselines and thresholds remain unchanged.

Only the exact shared value algorithm `algorithm:704` moves to implemented.
The input element, text/search state, other attributes and complete appearance
remain partial. The final audit retains all 17,267 unique IDs, with 2,240
implemented, 4,629 partial, 2,274 planned and 421 print-related excluded records,
plus 7,703 preserved non-printing records.


## Batch 18: HTML attribute-value equality

The pinned 2026-10-05 HTML selector rules, `review-source-2392` and
`review-source-2393`, define a fixed list of attribute names whose values compare
without ASCII case distinctions on HTML elements. `HtmlValidator` owns that rule
for the supported presence/equality selectors. Style delegates to it and removes
its former exact-comparison helper. Exact matches return without case folding;
other values retain their authored bytes. Only ASCII letters fold. Non-ASCII
letters, value, ID, class, name, role, title and data attributes remain exact.
Foreign-element and namespaced-attribute values remain exact too.

Case folding reserves its byte work through the existing `max_css_work` budget
before allocation. Tests cover every name in the pinned list, Unicode differences,
whitespace, missing/empty attributes, foreign namespaces and qualified attributes.
Public rendering tests compare byte-identical equivalent authored colors in both
form modes and check actionable diagnostics when case folding exhausts the budget.
The DOM validator also rejects non-string or invalid UTF-8 attribute names/values
in caller-supplied trees before selector matching, with a shared style diagnostic.
Malformed attribute namespace maps receive a separate actionable diagnostic.
These checks remain in the validator, and downstream matching uses their result.

The existing search-input fixture retains its source and thresholds. It now
changes 0.276% of host pixels, compared with 0.747% before the equality fix.
Original-value selectors still distinguish authored newlines from literal values.
This resolves the batch-17 type-value disagreement. Explicit `i`/`s` selector
flags, further selector operators, namespace name matching, quirks handling and
full foreign-content style integration remain unfinished. No broad selector row
is marked complete. Acceptance and performance evidence follow below.


The final matrix passes in `.quality/logs/20261008T011753Z-632514/`.
Both supported runtimes pass 1,129 tests, 100% coverage, 98 browser comparisons,
performance checks and Dialyzer, with no warnings. The exact host parity command
passes in `.quality/batch-18-host-parity-final.log`. The first matrix passed tests
but missed one direct non-string link-validator branch after earlier DOM rejection;
focused validator coverage restores that contract. Failed logs remain retained.

All 26 captures are consolidated in
`.quality/performance/results/1.1.0-batch-18/reports.json`. All 19 ordinary input
hashes and previous/candidate output sizes match. The whole-suite 1.20 row render
is 12.63% slower than the previous batch, while alternating the two changed
modules gives -0.16% on 1.20 and -7.03% on 1.19. This timing increase does not
reproduce in the isolated comparison. Other isolated ordinary timings remain
within 4.22% of the previous batch, with matching PDF hashes. Required attribute
preflight adds roughly 140,000 reductions to the row workload, 1.11%/1.20%,
and roughly 54,000 to table headings, 1.12%/1.15%. It checks caller-supplied trees
once before matching and prevents crashes on malformed attribute values/maps.
Skipping those checks would lose the supported failure diagnostics; trusting an
unverified tree marker would bypass validation. Fixed-baseline timing uncertainty,
including the whole-suite 1.20 sticker result at +5.08%, remains open for release.

Fresh-VM case scaling uses five warmups and 31 samples per variant/reference.
At 1,000 static inputs, warm medians are 93.457/92.113 ms on 1.19/1.20,
first-render times 167.167/157.897 ms, sampled VM peaks 85,157,552/102,232,704
bytes and output size 94,865 bytes. At 500 interactive inputs, warm medians are
279.817/264.366 ms, first-render times 288.304/263.191 ms, sampled VM peaks
109,408,792/101,545,840 bytes and output size 928,382 bytes. Every case-variant
PDF matches its exact-case and authored-text equivalents, including expected
logical text or field values. One cold sample per size/mode/runtime remains
exploratory; sampled VM memory excludes native allocations and brief peaks.
Limits, baselines and thresholds remain unchanged. The inventory audit preserves
all 17,267 unique IDs and the previous category counts; the partial selector rows
now describe the delivered equality rule and remaining work.


## Batch 19: Attribute operators and case flags

The pinned CSS Selectors Level 4 draft dated 22 January 2026, sections 6.1-6.3,
defines token, dash, prefix, suffix and substring operators and explicit `i`/`s`
flags. The local source is `tmp/performance/1.1.0/batch-19/selectors-4.html`,
SHA-256 `5d3b3f7e1fcc562dcec1a01ad2c5651e9f0bac294b255a75352e9df202604e6d`.
`HtmlValidator` owns the attribute grammar and comparison rules. `CssParser`
delegates to it and removes its former quoting helper. Existing presence/equality
ASTs remain intact; explicit flags add one comparison-mode field. Operator scans
reserve byte work through `max_css_work` before folding or token allocation.

Tests in existing files cover every operator and flag casing, ASCII-only folding,
CSS whitespace versus vertical tabs and Unicode separators, empty queries,
missing attributes, dash boundaries, foreign values, negation, cascade,
complete PDF equivalence and public syntax/resource diagnostics. Invalid bare
numeric values now fail instead of being accepted as CSS identifiers. Complete
CSS escapes, namespace names and quirks remain unfinished; no broad selector
inventory row moves to implemented.

The direct browser fixture first changed 2.575% of pixels because Chromium
ignores explicit `s` selectors. The final fixture compares their standard-based
native behavior with equivalent exact data selectors in Chromium. Its three
marked selectors receive `s`/`S` only in the native adapter, following the same
reference approach used for unsupported mathematical glyph features. Content,
style, page size and thresholds stay fixed. Host changed pixels are 0.609%.
The initial focused run also used the still-unsupported global `lang` attribute;
public cases now use supported data attributes, while validator tests cover the
HTML default rule directly. Initial failures remain retained. The first matrix
1.20 parity also raced with the concurrent host command while both cleaned shared
artifact directories. Two missing-file failures prompted a sequential full
matrix rerun with unchanged source. The host passed all 99 comparisons.
Acceptance and performance evidence follow below.


The sequential full matrix passes in `.quality/logs/20261008T041905Z-152174/`.
Both runtimes pass 1,136 tests, 100% coverage, 99 browser comparisons,
performance guards and Dialyzer, with no warnings. The exact host command passes
all 99 comparisons in `.quality/batch-19-host-parity-final.log`. The inventory
audit preserves all 17,267 IDs and the existing category counts. Performance evidence follows below.


All 36 captures are consolidated in
`.quality/performance/results/1.1.0-batch-19/reports.json`, with original raw
captures and their hashes retained. Twelve ordinary comparisons retain fixed
`fee6c4d` and accepted batch 18, twelve measure operator scaling, two alternate
changed modules, four repeat all 19 ordinary operations with their exact options,
four investigate two workloads in fresh processes and two count function calls.
All 19 ordinary input hashes and previous/candidate output sizes match.

Whole-suite timings vary sharply in both directions. The first 1.19 extraction
comparison is +70.88%, despite no changes to extraction; the initial isolated
comparison is within 2.40% across selected workloads. Two longer full-suite VMs
per runtime, five warmups and 61 samples, expose an extraction median gap of
32.28%/35.45% on 1.20 and report gaps of 8.02%/10.70%. Extraction samples have two
timing bands. A 31/30 split around their midpoint flips the median, while the
mean changes +0.87%/-0.11%. A read-only observation also records substantial CPU
pressure and a CPU-heavy VS Code BEAM process, without changing its settings.
These observations identify confounders rather than prove every timing cause.

The follow-up repeats both workloads in two fresh VMs per runtime. Each sample
uses its own process, five warmups and a collection before timing, with GC
statistics recorded. Report medians stay within 1.26%; extraction stays within
0.76%, with reductions within 0.013% and every output hash matching. Call-count
profiles show identical report calls and zero calls to either changed module
from extraction, with identical Text/Reader/validator counts. The report never
calls the new attribute functions. The repeated large median gaps do not survive
the process-isolated comparison; original captures remain retained. The image
lacks optional `cprof`, so the profiler uses verified built-in call-count
breakpoints without changing dependencies. Other full-suite timing outliers do
not repeat consistently. Fixed-baseline and exploratory cold uncertainty remains
open for final release review, with no threshold or baseline reset.

Operator scaling compares six comparisons per input with equivalent inline blue
styles and equivalent text controls, using fresh VMs, five warmups and 31 samples.
At 1,000 static controls, warm medians are 177.123/119.391 ms on 1.19/1.20,
first renders 181.753/176.660 ms, sampled VM peaks 89,272,328/83,673,376 bytes
and output size 94,865 bytes. At 500 interactive controls, warm medians are
382.989/482.437 ms, first renders 477.857/495.261 ms, sampled VM peaks
110,453,088/109,034,000 bytes and output size 928,382 bytes. Six selectors add
8.935/9.154 ms versus inline colors at that size, with 85,387/110,895 additional
reductions. Static selector timings are below the inline reference in these
samples. Each first/repeated PDF matches its references byte for byte, with all
logical text or field values retained. One cold sample per size/mode/runtime is
exploratory; VM sampling excludes native allocations and brief peaks. Limits,
source fixtures and thresholds remain unchanged.


## Batch 20: Readonly text controls

`HtmlValidator` accepts boolean readonly on text/search inputs and textareas,
including minimized, uppercase, empty and `readonly="false"` values. It owns
applicability for parsed HTML and caller-supplied style trees. Other input types
and hosts still reject the attribute. `HtmlFormValidator` sets read-only bit 1
once when readonly or disabled is present and preserves textarea multiline bit
4096. Writer, appearance and form APIs retain their existing paths. Readonly adds
no disabled appearance; original values remain visible to selectors. Filling
returns the existing actionable read-only diagnostic and flattening still works.

Tests in existing files verify parsing, applicability, original attributes,
independent disabled styles, values, exact flags, fill diagnostics, flatten text,
static PDF equivalence and public source locations. The initial focused failure
asserted diagnostics from the legacy bare-error Style wrapper; the corrected
assertion uses its existing detailed API. All 296 focused tests pass. The new
`html_readonly_controls.html` fixture changes 0.238% of host pixels on one page.
The matrix passes in `.quality/logs/20261008T050121Z-283095/`, with 1,143 tests,
100% coverage, 100 browser comparisons, performance and Dialyzer on both supported
runtimes, and no warnings. The sequential exact host command passes all 100
comparisons in `.quality/batch-20-host-parity-final.log`.

All 38 captures are consolidated in
`.quality/performance/results/1.1.0-batch-20/reports.json`, with original bytes and
hashes retained. Six captures affected by accidentally overlapping pinned-CPU
benchmark runs are explicitly excluded and rerun sequentially. The first
interactive feature probe accessed internal flags on the public fields map;
the corrected benchmark uses the existing Reader/FormValidator inspection API.
No library changes came from those benchmark corrections.

The twelve ordinary captures preserve all 19 input hashes and previous/candidate
output sizes. Whole-suite previous timing increases stay within 2.70%. Exact
operation alternation retains every output hash, but the 1.19 extraction median
rises 24.01% and the 1.20 full-font row median rises 23.95%. Fresh-process
follow-ups with identical warmups place extraction within 0.16% and the full-font
row within 1.04% on both runtimes, with reductions within 0.013%. Call-count
profiles show zero extraction calls to either changed module and identical
Text/Reader/validator counts; full-font row totals also match. The large gaps do
not survive process isolation. The fixed-baseline 1.19 table timing is +8.06% and
1.20 direct boxes +5.64% in these whole-suite captures. Fixed-baseline timing and
exploratory cold uncertainty remain open for final release review. No baseline,
threshold, input, output requirement or resource limit is reset.

Fresh-VM readonly scaling mixes text/search/textarea controls. At 1,000 static
controls, warm medians are 112.566/120.710 ms on 1.19/1.20, first renders
176.741/170.824 ms, sampled VM peaks 83,523,600/87,844,296 bytes and output
size 105,823 bytes. Every static PDF matches its editable reference byte for
byte. The readonly attribute adds parsing/applicability work, measured separately
at 569,601/620,768 reductions and 2.396/18.914 ms at that size. At 500 interactive
controls, warm medians are 281.103/274.988 ms, first renders 377.823/283.418 ms,
sampled VM peaks 122,336,664/117,912,840 bytes and output size 946,298 bytes.
Interactive hashes intentionally differ from editable fields because flags differ;
all values, exact flags, read-only fill diagnostics and flattened logical text
pass. Each repeated PDF matches its expected kind. One cold sample is exploratory;
VM sampling excludes native allocations and brief peaks.

The inventory audit preserves all 17,267 unique IDs. Three broad readonly records
move from planned to partial, while textarea notes describe the delivered flags
and remaining work. Counts are 2,240 implemented, 4,632 partial, 2,271 planned,
421 print exclusions and 7,703 preserved non-printing records. Other applicable
input types, constraints, ownership, complete native themes and the fractional
widget raster gap remain unfinished. This batch does not complete milestone 1.1.0.


## Batch 21: Telephone, URL and single-email values

The pinned HTML algorithms 705, 706 and 709 define initial sanitation. Telephone
strips CR/LF while URL and single email also trim ASCII edge whitespace. Non-ASCII
whitespace, vertical tabs, internal spaces and telephone punctuation remain intact.
`HtmlValidator` owns one supported text-state classification used by Style and
HtmlFormValidator. Its existing metadata ASCII trimming is shared with the new
states through a private helper. Original attributes remain available to CSS;
painting and AcroForm values consume sanitized values. Readonly applies to all
supported text-entry states. Generated names remain `TEXT`, and invalid URL/email
initial strings still render without invented constraint failures.

Existing tests verify sanitation distinctions, case variants, empty content,
original selectors, exact PDF equality to text controls, initial values, flags,
names, filling/flattening and actionable unsupported-attribute diagnostics.
The isolated preview exposed old email rejection assertions and incorrect test
assumptions about Style options/styled attributes; corrected tests use the existing
API and original parsed tree. All 302 focused tests pass in the main checkout.
`html_contact_inputs.html` changes 0.661% of host pixels on one page, with the
existing author geometry and thresholds. The matrix passes in
`.quality/logs/20261008T052302Z-364546/`, with 1,149 tests, 100% coverage, 101
browser comparisons, performance and Dialyzer on both supported runtimes, and no
warnings. The exact sequential host command passes all 101 comparisons.

All 26 captures are consolidated in
`.quality/performance/results/1.1.0-batch-21/reports.json`, with raw bytes and
hashes retained. Twelve preserve the fixed fee6c4d/accepted batch-20 comparisons
in both orders and runtimes. Twelve measure new state scaling, and two alternate
only the three changed modules across all 19 exact ordinary operations/options.
The alternating samples use fresh processes after one untimed operation and
collection; five initial warmups per revision precede the 31 samples. Spawning,
loading, warming and collection occur outside timing, with GC statistics recorded.
This controls the inherited process heap state that distorted earlier batches.

All ordinary input hashes and previous/candidate output sizes match. Whole-suite
timing increases remain below 5% versus the previous batch. The fresh-process
comparison retains every output hash, with maximum timing increases
1.58%/1.54% and reduction increases 0.032%/0.037% on 1.19/1.20. Fixed-baseline
whole-suite 1.19 row timing is +13.19% and 1.20 tables +5.04%; final fixed-baseline
and cold uncertainty remains open, without changing any input, limit or threshold.

Fresh-VM scaling alternates mixed telephone/URL/single-email controls with exact
sanitized text references, including readonly fields. At 1,000 static controls,
warm medians are 95.264/85.325 ms, first renders 170.573/172.199 ms, sampled VM
peaks 89,139,592/77,318,040 bytes and output size 94,887 bytes. At 500 interactive
controls, warm medians are 286.593/260.531 ms, first renders 373.326/331.552 ms,
sampled VM peaks 119,620,672/100,897,936 bytes and output size 928,382 bytes.
Every first and repeated PDF matches its reference, with exact expected spans,
field values and readonly status. Additional sanitation work is measured
separately. One cold sample remains exploratory; VM memory excludes native
allocations and brief peaks.

Only the three exact sanitation algorithms move to implemented. The broader
state sections move planned to partial and type/readonly notes describe the
new subset. The audit preserves all 17,267 unique IDs, with 2,243 implemented,
4,635 partial, 2,265 planned, 421 print exclusions and 7,703 non-printing records.
Multiple email values, IDN display conversion, constraints, other attributes,
ownership, full native themes and fractional widget raster parity remain open.
The full 1.1.0 plan remains unfinished.

## Batch 22: Multiple-email initial lists

The pinned HTML algorithm 713 defines initial email-list sanitation. The validator
splits on commas, trims ASCII token edges and rejoins in original order. Empty
tokens remain; internal CR/LF inside invalid tokens is preserved, as this algorithm
requires. Boolean presence applies to `multiple="false"`. HtmlValidator owns
applicability for parsed HTML and caller-supplied trees. Other supported input
types and listboxes still reject multiple. Style, HtmlFormValidator and writer
retain the existing text-field path, string values and readonly flags. No PDF
multiple-choice flag or structured-list filling API is introduced.

Existing tests verify order, empty tokens/controls, whitespace distinctions,
internal newlines, case/presence variants, original selectors, exact text-equivalent
PDFs, values, flags, filling, flattening and diagnostic locations. The initial
preview expected a textarea failure to include its contents in `source`; the
correct assertion checks its opening token and exact line/column. All 307 focused
tests pass in the main checkout. `html_multiple_email.html` changes 0.250% of host
pixels on one page. The matrix passes in `.quality/logs/20261008T054108Z-435462/`,
with 1,154 tests, 100% coverage, 102 browser comparisons, performance and Dialyzer
on both supported runtimes, and no warnings. The exact sequential host command
passes all 102 comparisons.

All 28 captures are consolidated in
`.quality/performance/results/1.1.0-batch-22/reports.json`, retaining raw bytes and
hashes. Twelve ordinary captures preserve fixed fee6c4d and accepted batch 21 in
both orders/runtimes; twelve measure field scaling, two alternate the changed
validator across exact ordinary operations in fresh processes, and two measure
increasing token counts without PDF layout. Input hashes and previous/candidate
ordinary output sizes match, and every alternating output hash matches.

Whole-suite 1.19 row timing rises 18.38%, and 1.20 purchase order/trim timings rise
6.13%/8.43%. Fresh-process alternation does not reproduce these increases.
Its maximum timing increases are 3.47%/1.17%, and maximum reduction increases
0.025%/0.025% on 1.19/1.20. The 3.47% result belongs to the unchanged direct-box
writer control. Fixed-baseline whole-suite row/table/box timing uncertainty stays
open for final review. No source fixture, option, baseline, threshold, output
requirement or resource limit changes for acceptance.

Fresh-VM field scaling compares two email tokens per control with exact sanitized
text references, including readonly variants. At 1,000 static controls, warm
medians are 111.324/101.771 ms, first renders 180.429/172.366 ms, sampled VM peaks
78,377,008/82,700,680 bytes and output size 95,640 bytes. At 500 interactive
controls, warm medians are 294.561/273.678 ms, first renders 293.922/273.306 ms,
sampled VM peaks 118,761,984/122,330,952 bytes and output size 956,541 bytes.
All first/repeated PDFs match their text reference, with expected spans, string
values and readonly status. Additional attribute/token work is measured separately.
One cold sample is exploratory, and sampled VM memory excludes native/brief peaks.

The pure validated-map benchmark checks 1/100/1,000/10,000 tokens, five warmups
and 31 samples, with exact order/output hashes. At 10,000 tokens and 278,893 input
bytes, medians are 21.785/20.809 ms and 760,636/760,637 reductions. Sampled VM
peaks during separate 100-operation loops are 61,129,512/62,335,560 bytes.
This measures initial sanitation and allocation, not long-control PDF geometry.

Only algorithm 713 moves to implemented. Three broader multiple records move
planned to partial; email/type/readonly notes describe delivered behavior and
remaining work. The audit preserves all 17,267 unique IDs, with 2,244 implemented,
4,638 partial, 2,261 planned, 421 print exclusions and 7,703 non-printing records.
Constraint/list validity, IDN display conversion, ownership, other attributes and
hosts, complete native appearance and fractional widget parity remain unfinished.
The full 1.1.0 plan is still incomplete.


## Batch 23: Input type enumeration and defaults

HtmlValidator owns the complete fixed keyword vocabulary and ASCII recognition.
Missing, empty, unknown, obsolete, untrimmed and non-ASCII lookalike values select
Text. Recognized states whose renderer is unfinished continue to fail. Values
longer than the longest keyword select Text without folding or copying; this is
fixed vocabulary recognition, not a configurable input ceiling. Original attributes
remain available to CSS. Style and HtmlFormValidator delegate effective state,
sanitation, Text mapping and readonly applicability to the validator. Caller input
trees now receive canonical type, checked/multiple/readonly and empty-child checks.
Broader caller attribute validation remains unfinished.

All 313 focused tests pass in the main checkout. The new author-styled fixture
`html_input_type_defaults.html` changes 0.378% of host pixels on one page. The matrix
passes in `.quality/logs/20261008T060117Z-506058/`, with 1,160 tests, 100% coverage,
103 browser comparisons, performance and Dialyzer on both supported runtimes and
no warnings. The exact sequential host command passes all 103 comparisons.

All 38 captures are consolidated in
`.quality/performance/results/1.1.0-batch-23/reports.json`, retaining raw bytes and
hashes. The interruption stopped the driver after four complete ordinary captures;
those reports were verified and retained, then the remaining work resumed without
concurrent captures or source mutation. Twelve ordinary captures preserve the fixed
fee6c4d/accepted batch-22 comparisons in both orders and runtimes. Twelve measure
new state scaling, two alternate the three changed modules across the exact 19
ordinary operations in fresh processes, and twelve measure large unknown type
attributes with real CSS prefix/suffix matching. Input hashes and previous/candidate
ordinary sizes match; every alternating output hash matches.

Whole-suite 1.19 invoice timing rises 6.21%. On 1.20, full-font rows rise 8.01%,
tables 6.75%, and PDF extraction timings 5.26%/6.69%. Fresh-process alternation does
not reproduce those timing increases. Its maximum timing increases are 1.82%/2.42%
and maximum reduction increases 0.035%/0.040% on 1.19/1.20. Fixed-baseline table,
full-font row and extraction timing uncertainty remains open for final review.
No fixture, option, baseline, threshold, output requirement or limit changes.

At 1,000 static controls, warm medians are 110.225/82.613 ms, first renders
182.749/159.605 ms, sampled VM peaks 83,624,744/83,684,328 bytes and output size
94,887 bytes. At 500 interactive controls, warm medians are 282.436/285.448 ms,
first renders 354.505/348.112 ms, sampled peaks 106,365,248/118,462,432 bytes and
output size 928,382 bytes. Every first/repeated PDF matches its sanitized Text
reference, with exact spans, names, values and readonly status.

Large-attribute captures retain up to 1,000,008 original type bytes, render under
default limits and match complete blue Text-reference PDFs selected by two CSS
operators. Their scans and memory remain measured costs, not discarded data.
First samples remain exploratory; sampled total VM memory excludes native and
brief peaks. Final cold-release acceptance is still open.

Only algorithm 684 moves partial to implemented. Broader input/type/readonly rows
remain partial. The audit preserves all 17,267 unique IDs, with 2,245 implemented,
4,637 partial, 2,261 planned, 421 print exclusions and 7,703 non-printing records.
The full 1.1.0 plan remains unfinished.


## Batch 24: Initial input-button labels

Submit/reset/button inputs render static artwork in both form modes. HtmlValidator
owns the supported keywords and initial missing-label defaults, using English
Submit/Reset/empty labels. Explicit empty values and whitespace remain intact.
CSS sees the original attributes before semantic preparation. Style reuses button
appearance, centered text and border-box defaults, with author overrides. Input
labels use pre whitespace; empty input buttons retain a line of height. Empty
button elements print no submission-value fallback and keep collapsed geometry.
Internal input-button and button-element kinds distinguish those cases. Buttons
create no fields, reserve no names and consume no appearance-widget budget.

Focused tests in the main checkout pass all 468 cases, including exact button
PDF equivalence, values, original selectors, disabled presence, multi-page content,
empty block/flex/grid height, field limits/names, fill/flatten and diagnostics.
The initial preview tests had expectation mistakes for styled text, the previous
incorrect button value fallback and the reader's field-tree widget count. One
accidental main-checkout preview run is retained but excluded from acceptance.

The first fixture changed 3.236% because button elements received input-like empty
height. Distinguishing their internal kinds fixes the same fixture to 0.0568%.
Adding whitespace cases exposed input-label pre formatting; those cases remain
in the final one-page fixture, which changes 0.801% without loosening thresholds.
The matrix passes in `.quality/logs/20261008T063042Z-601138/`, with 1,167 tests,
100% coverage, 104 browser comparisons, performance and Dialyzer on both runtimes,
without warnings. The exact sequential host command passes all 104 comparisons.

All 26 captures are consolidated in
`.quality/performance/results/1.1.0-batch-24/reports.json`, retaining raw bytes and
hashes. Twelve preserve fixed fee6c4d/accepted batch-23 comparisons in both orders
and runtimes, twelve measure new state scaling, and two alternate the three changed
modules across all 19 exact ordinary operations in fresh processes. Input hashes,
previous/candidate ordinary sizes and every alternating output hash match.

Whole-suite 1.19 row timing rises 26.02%, and 1.20 purchase order/large PNG timings
rise 8.58%/11.62%. Fresh-process alternation does not reproduce these increases.
Its maximum timing increases are 0.73%/1.94%, and maximum reduction increases
0.023%/0.042% on 1.19/1.20. The 1.20 maximum timing result is the unchanged direct
box writer control. Fixed-baseline table and row timing uncertainty stays open for
final review. No fixture, option, threshold, baseline or resource limit changes.

At 1,000 static buttons, warm medians are 186.754/158.358 ms, first renders
249.771/225.183 ms and sampled VM peaks 76,855,776/86,571,776 bytes. At 1,000
buttons in interactive mode, warm medians are 173.364/156.600 ms, first renders
243.328/227.273 ms and peaks 86,796,088/85,197,072 bytes. All first/repeated
PDFs match exact button-element references, retain expected label spans and have
no fields. Output size is 113,734 bytes. Equal author heights make empty button
reference geometry comparable. One cold sample is exploratory, and sampled VM
memory excludes native and brief peaks. Additional input semantics are measured
separately from the ordinary suite.

Only three exact initial-label requirements and the centered-text UA declaration
move to implemented. Submit's broader state moves planned to partial; other type,
element and rendering rows stay partial. The audit preserves all 17,267 IDs, with
2,249 implemented, 4,635 partial, 2,259 planned, 421 print exclusions and 7,703
non-printing records. Additional attributes, ownership, full native sizing/themes,
long-label clipping and the wider 1.1.0 plan remain unfinished.

## Batch 25: Initial enabled and disabled selectors

HtmlValidator owns initial enabled/disabled classification for supported HTML
input, button, select, textarea and option hosts. Disabled is a boolean attribute,
so `disabled="false"` still disables. Readonly and ARIA alone leave controls
enabled; ordinary elements and foreign nodes match neither state. CssParser
delegates simple pseudo-class keywords to the validator, using ASCII case matching.
Style calls the state predicate only when a selector uses it. Negation, existing
positional selectors, specificity and hidden-input precedence remain intact.

All 273 focused tests pass. The new one-page control-state fixture changes 0.332%
of host pixels. The matrix passes in `.quality/logs/20261008T064936Z-668703/`,
with 1,172 tests, 100% coverage, 105 browser comparisons, performance and Dialyzer
on both supported runtimes, without warnings. The exact sequential host command
passes all 105 browser comparisons.

All 26 sequential captures are consolidated in
`.quality/performance/results/1.1.0-batch-25/reports.json`, retaining raw bytes and
hashes. Twelve preserve fixed fee6c4d/accepted batch-24 comparisons in both orders
and runtimes. Twelve measure the new selectors against exact class-selector
references. Two alternate the three changed modules across all 19 ordinary
operations in fresh processes. Input hashes and previous/candidate ordinary sizes
match; every alternating output hash matches.

Whole-suite timing rises 7.65% for the 1.19 inline paragraph, and 24.59%/6.34% for
1.20 HTML rows/text extraction. Fresh-process alternation does not reproduce those
increases. Its maximum timing increases are 3.82%/1.22%, and maximum reduction
increases 0.037%/0.023%. The 1.19 timing maximum is the small stock sticker, whose
reductions rise 484, or 0.037%. Fixed-baseline timing and final cold acceptance
remain open. No output, option, fixture, threshold, baseline or resource limit
changes are used to pass the ordinary checks.

At 1,000 static controls, warm medians are 117.181/107.725 ms, first renders
169.686/163.378 ms and sampled VM peaks 96,578,128/83,396,816 bytes. Output is
95,002 bytes. At 500 interactive controls, warm medians are 275.144/271.546 ms,
first renders 284.906/270.306 ms and peaks 106,185,024/108,615,360 bytes. Output
is 928,382 bytes. Every first/repeated PDF matches its complete class-selector
reference, with expected static spans, interactive values and readonly flags.
One first-render sample is exploratory; sampled VM memory excludes native and
brief peaks.

Four broad selector records move planned to partial, preserving their remaining
1.2.0 work. Fieldsets, optgroups, ancestor disabling and form-associated custom
hosts remain unfinished. The audit preserves all 17,267 IDs, with 2,249 implemented,
4,639 partial, 2,255 planned, 421 print exclusions and 7,703 non-printing records.
The full 1.1.0 plan remains unfinished.

## Batch 26: Initial image dimension hints

HtmlValidator preserves original img width/height attributes and owns their HTML
dimension parsing. Initial ASCII digits, optional fractions and immediate percent
signs provide pixel/percentage hints; invalid prefixes are ignored. Existing
numeric token and magnitude limits apply before image assets are resolved. Style
merges these hints below author CSS. CSS width/height:auto uses the loaded natural
ratio. Shared CSS size parsing moves from Style to HtmlValidator, including existing
percentage/minimum-size behavior. Layout preserves zero extents in block/flex/grid
flow without creating an invalid empty image paint box. Asset checks still run.

All 313 focused tests pass in staging and the main checkout. Exact PDF comparisons
cover all object-fit modes, dimensions, author overrides and zero extents. Public
failure tests cover numeric excesses, denied assets and invalid CSS. The one-page
fixture changes 0.647% of host pixels; both rendered pages retain every label and
image case. Initial test mistakes concerned the PNG support loader, resolver request
shape, asset error reason and error ordering. An accidental unchanged-source matrix
invocation was stopped before application and excluded from acceptance.

The first applied matrix passed tests, coverage and performance but rejected an
incomplete CSS size-context type specification. Including existing minimum/maximum
callers fixes the same implementation. The final matrix passes in
`.quality/logs/20261008T093723Z-825487/`, with 1,179 tests, 100% coverage, 106
browser comparisons, performance and Dialyzer on both supported runtimes, without
warnings. The exact sequential host command passes all 106 browser comparisons.

All 26 sequential captures are consolidated in
`.quality/performance/results/1.1.0-batch-26/reports.json`, retaining raw bytes and
hashes. Twelve compare fixed fee6c4d, accepted batch 25 and this batch in both
orders and runtimes. Twelve compare dimension hints against complete CSS-reference
PDFs at 1/100/1,000 repeated images in both form modes. Two alternate the three
changed modules across all 19 ordinary operations in fresh processes. Input hashes,
previous/candidate ordinary sizes and every alternating output hash match.

Whole-suite 1.20 purchase-order timing rises 7.10%; fresh-process alternation changes
that operation by -0.37%. All other whole-suite previous timings rise less than 5%.
Fresh-process maximum timing increases are 0.89%/1.07%, and maximum reduction
increases 0.069%/0.027% on 1.19/1.20. No ordinary input, option, threshold, baseline
or limit changes are used to pass checks. Final cold/fixed-baseline acceptance
remains open.

At 1,000 images in static mode, warm medians are 194.709/179.210 ms, first renders
227.167/215.261 ms and sampled VM peaks 93,508,584/100,540,224 bytes. In interactive
mode they are 198.644/188.093 ms, 223.727/216.078 ms and 95,195,160/88,735,488 bytes.
Every first/repeated PDF matches the complete CSS equivalent, retains all 1,000
text labels and contains no fields. Both modes produce 103,176 bytes. A repeated
PNG, percentage/fraction hints, invalid hints, author overrides and zero extents
are measured together. First samples are exploratory; sampled VM memory excludes
native and brief peaks.

Twelve broad image/dimension records receive current evidence while staying
partial. Responsive selection, other dimension hosts, placeholder ratios and
complete alternative text remain unfinished. All 17,267 IDs are preserved, with
2,249 implemented, 4,639 partial, 2,255 planned, 421 print exclusions and 7,703
non-printing records. The full 1.1.0 plan remains unfinished.


## Batch 27: Initial password fields

Password inputs now use canonical Text-field preparation, remove initial CR/LF
without trimming, and paint one U+2022 bullet per grapheme. Original attributes
remain available to CSS. Interactive Text fields retain sanitized values with
Password bit 8192 and optional readonly bit 1. The shared FormValidator masks new
fill appearances and reserves their actual bytes against existing budgets.
Comb and rich-text flags remain unsupported. Flattening preserves current artwork;
stored values and earlier revisions remain data, so masking is not redaction.

Nine focused tests extend seven existing files. The main focused run passes 359
checks. Static PDFs match literal bullet references, while interactive values,
flags, masked fill/flatten output, external password fields, readonly failures and
appearance allocation limits are checked independently. The new one-page browser
fixture changes 0.120% of host pixels and retains all cases, including composed
accents, supplementary scalars, a family sequence and a flag. Both pages were
inspected. An initial matrix exposed an old unsupported-password expectation in
FormValidation; updating it and adding external-field coverage resolves that failure.

The final matrix passes in `.quality/logs/20261008T100214Z-897053/`, with 1,188
tests, 100% coverage, 107 browser comparisons, performance and Dialyzer on both
supported runtimes, without warnings. The sequential exact host browser command
passes all 107 comparisons.

All 30 sequential captures are consolidated under
`.quality/performance/results/1.1.0-batch-27/reports.json`, retaining original raw
bytes and hashes. Twelve compare fixed fee6c4d, accepted batch 26 and this batch
in both orders and runtimes. Twelve measure initial password creation at
1/100/1,000 static and 1/100/500 interactive fields. Four measure password
fill/flatten at 1/100 fields. Two alternate the four changed modules across the
original 19 ordinary operations plus 100-field ordinary Text fill/flatten in fresh
processes. All ordinary input hashes, previous/candidate sizes and alternating
output hashes match. The exploratory host pilot is excluded.

Whole-suite 1.19 repeated-large-PNG timing rises 8.85%; fresh-process alternation
changes that operation by 0.055%. All other whole-suite previous timings rise
less than 5%. Fresh-process maximum timing increases are 1.023%/2.719%, and maximum
reduction increases 0.045%/0.021% on 1.19/1.20. Ordinary Text fill changes
-0.300%/-0.316%, and flatten -1.635%/+0.584%, with identical complete PDFs.
No ordinary input, option, threshold, baseline or limit changes are used to pass
checks. Final cold/fixed-baseline acceptance remains open.

At 1,000 static password fields, warm medians are 80.391/85.121 ms, first renders
151.171/145.783 ms and sampled VM peaks 85,193,632/83,680,976 bytes. Output is
95,127 bytes. At 500 interactive fields, warm medians are 269.064/249.167 ms,
first renders 332.062/328.228 ms and peaks 123,659,568/122,229,496 bytes. The
reference output is 908,965 bytes. Static PDFs match the full bullet equivalent.
Interactive PDF object graphs match after removing only independently checked
field V/Ff differences; every other object, appearance, font and page stream matches.
Every repeated kind retains its exact first PDF bytes.

For 100 password fields, fill medians are 402.326/403.784 ms and flatten medians
71.186/68.771 ms. Fill peaks are 262,525,576/256,206,792 bytes; flatten peaks
70,993,136/72,306,376 bytes. The complete normalized object graphs match reference
Text-field operations, and flatten removes fields while retaining all bullets.
These API first samples follow initial creation and verification in the same VM,
so they are not fresh-process cold measurements. Creation first samples are
exploratory; sampled VM memory excludes native and brief peaks.

The exact password value-sanitization algorithm moves to implemented. Four broad
password records move planned to partial, and existing broad notes retain remaining
attributes, constraints, ownership and native appearance work. The audit preserves
all 17,267 IDs, with 2,250 implemented, 4,643 partial, 2,250 planned, 421 print
exclusions and 7,703 non-printing records. The full 1.1.0 plan remains unfinished.

## Batch 28: Initial script data blocks

HtmlValidator owns initial type classification and escaped/double-escaped script
boundaries. MimeValidator shares the fixed JavaScript MIME essence vocabulary.
Exact empty type selects classic; nonempty whitespace selects data after ASCII
trimming, and MIME parameters do not match executable essences. Legacy language
defaults retain their specified handling. HtmlParser preserves literal data without
entity decoding, HTML/CSS parsing, format validation or resource resolution. Script
boxes default hidden; author CSS can expose raw text in ordinary body contexts.
Leading metadata scripts join the constructed head. Executable kinds reject with
source diagnostics, including hidden and directly supplied DOM input. Foreign
script rejection remains unchanged.

Seven focused tests extend five existing files. All 325 focused checks pass in
staging and root. Tests preserve raw DOM data and selector attributes, compare full
PDFs to literal references and check source/text limits and executable failures.
The one-page fixture changes 0.793% of host pixels, retaining every boundary/data
case. Both rendered pages and extracted positions were inspected. The first matrix
passed tests but exposed two uncovered strict parser boundaries; adding incomplete
closing-token and unsupported closing-tag cases resolves the coverage gate.

The final matrix passes in `.quality/logs/20261008T103723Z-973599/`, with 1,195
tests, 100% coverage, 108 browser comparisons, performance and Dialyzer on both
supported runtimes, without warnings. Exact sequential host parity passes all
108 comparisons. These data-block comparisons do not contain executable scripts;
explicitly disabling Chromium scripting is being added in the next batch.

All 30 sequential captures are consolidated under
`.quality/performance/results/1.1.0-batch-28/reports.json`, preserving raw bytes
and hashes. Twelve compare fixed fee6c4d, accepted batch 27 and this batch in both
orders and runtimes. Twelve measure 1/100/1,000 hidden/visible data blocks against
complete equivalents. Four measure approximately 1 MB of plain and repeated
escaped data. Two alternate all four changed modules across the original 19
ordinary operations plus ordinary 100-field Text fill/flatten in fresh processes.
All ordinary input hashes, previous/candidate sizes and alternating output hashes
match. The host pilot is excluded.

Whole-suite 1.19 repeated-large-PNG timing rises 11.70%, and 1.20 HTML rows/large
PNG timing 13.47%/36.64%. Fresh-process alternation changes those operations by
0.290%, 0.292% and -0.090%, respectively. Its maximum timing increases are
1.541%/0.859%, with maximum reduction increases 0.051%/0.060% on 1.19/1.20.
Ordinary Text fill changes 0.011%/0.280%, and flatten -0.511%/+0.467%, with exact
complete PDF equality. No ordinary inputs, options, thresholds, baselines or limits
change to pass checks. Final fixed/cold acceptance remains open.

At 1,000 hidden blocks, warm medians are 113.277/133.200 ms, first renders
187.277/181.994 ms and sampled VM peaks 90,130,792/82,741,304 bytes. Output is
90,653 bytes and retains every visible label. At 1,000 visible blocks, warm medians
are 302.947/282.919 ms, first renders 395.667/364.348 ms and peaks
101,543,024/112,557,464 bytes. Output is 120,094 bytes. Every first/repeated full
PDF matches its complete reference, with original raw parser data independently
retained and no resolver calls. Hidden-data extra reductions are approximately
2.56 million for 1,000 blocks; visible-data extras approximately 1.14 million.
These features add parsing, classification and boundary work that the references
containing only labels or literal div text do not require.

One roughly 1 MB plain block takes 13.792/13.422 ms warm and 77.813/71.461 ms
first, with peaks 67,482,896/65,180,120 bytes. Repeated escaped transitions take
139.606/147.773 ms warm and 202.519/209.160 ms first, with peaks
62,388,824/62,726,040 bytes. Both preserve all raw data and match the complete
77,093-byte label reference PDF. First samples are exploratory; sampled VM memory
excludes native and brief peaks.

The exact script-type attribute classification moves to implemented. Legacy
language and the broad inert-data/composition decision move planned to partial.
Script tokenizer sections stay partial, and excluded browser-execution notes now
acknowledge static data acceptance. Specialized script-supporting list/table/select
construction, source preprocessing, templates and broader composition remain
unfinished. The audit preserves all 17,267 IDs, with 2,251 implemented, 4,645
partial, 2,247 planned, 421 print exclusions and 7,703 non-printing records.
The full 1.1.0 plan remains unfinished.

## Batch 29: Scripting-disabled browser references

Each comparison now creates a fresh Chromium profile with JavaScript denied through
its default content setting. Comparison records explicitly store
`scripting: :disabled`. A reference-only page contains noscript fallback and a
script that would replace its body. The probe verifies unchanged 1% pixel parity
against three ordinary paragraphs, extracts the browser PDF to confirm fallback
presence, and confirms the replacement text is absent. Production executable-input
rejection remains unchanged; native noscript is still unfinished.

The isolated probe passes. The root matrix passes in
`.quality/logs/20261008T105843Z-1041958/`, with 1,195 tests, 100% coverage, 109
browser comparisons, performance and Dialyzer on both supported runtimes, without
warnings. Sequential exact host parity passes all 109 comparisons. Every renderer
source hash matches accepted batch 28, so no new renderer performance delta or
benchmark bundle is claimed. The existing performance matrix retains its thresholds.
No inventory categories or IDs change. Prior comparisons without executable
scripts remain useful, but did not explicitly disable scripting. Ordinary templates,
parser-time replacement, shadows/slots and the full milestone remain unfinished.



## Batch 30: initial ordinary inert templates

Ordinary HTML templates retain empty actual children and separate
`document_fragment` content. Nested templates, literal script data, entities and
foreign namespaces remain in the parser tree. Unused contents do not collect
styles, fonts, assets, fields or metadata. Author CSS may paint an empty template
box; selectors still count the actual template element. Input policy, UTF-8 and
shared node/depth limits apply to unused fragments and direct DOM callers.
Executable content diagnoses before rendering. Active composition attributes
still diagnose. Complete insertion modes, specialized parent contexts,
owner-document semantics and active replacement/shadow/slot behavior are open.

Six checks extend existing parser/style/render files. The scripting-disabled
browser fixture passed at 0.149712% changed pixels on one page; both full pages
retain all visible labels and two empty styled boxes. Root validation passed
1,201 tests, 100% coverage, performance, Dialyzer and 110 browser checks on both
supported runtimes. The final matrix is
`.quality/logs/20261008T114239Z-1178193/`; exact host comparison passed all 110.
There are no final matrix FAIL/WARN entries. An unused internal default-argument
wrapper was removed after the first coverage failure. Foreign event validation
now avoids scanning ordinary HTML attributes and remains canonical for foreign
and inactive content. Earlier exploratory captures remain separately excluded.

The 34 captures are consolidated in
`.quality/performance/results/1.1.0-batch-30/reports.json`. Twelve fixed/previous/
candidate captures use the same ordinary inputs, options and both revision orders
on CPUs 4,5. All 19 input hashes and previous/candidate output sizes match. Two
fresh-process alternating captures retain every complete output hash across all
19 ordinary operations and ordinary 100-Text-field fill/flatten operations.
Only HtmlParser, Style, HtmlValidator and ForeignContentValidator differ.

The first Elixir 1.19 fresh comparison showed purchase-order +6.969% and table
+7.656% timing. Preserved follow-ups use 61 samples per revision and show
-0.657% and +0.907%. Elixir 1.20 shows purchase-order +2.263% initially and
+4.641% in the follow-up; its table follows +0.079% then -0.187%. These captures
remain distinct. Initial maximum added ordinary reductions were 0.759%/0.866%.
Canonical UTF-8 text validation, inactive-fragment shape/policy checks and template
branches account for new validation work. They return recoverable diagnostics
instead of allowing invalid unused input through. Ordinary foreign attribute
scanning was removed. Duplicate internal stylesheet preflight remains an
identified optimization for the next batch. Unchanged ordinary form fill is
+0.538%/+0.195% and flatten +0.307%/-0.887%, with identical PDFs. Final fixed and
cold milestone acceptance remains open.

Twelve separate complex-template captures cover 1/100/500 hidden and author-styled
instances, preserving every style, denied resource, field, nested template,
script and SVG/MathML node. Every first/repeated complete PDF equals its literal
empty-content reference. All labels remain; fields and resolver calls stay absent.
At 500 hidden instances, warm times are 175.657/178.371ms, exploratory first
renders 253.994/246.588ms and sampled VM peaks 77,747,344/73,940,784 bytes.
The 84,740-byte PDF is exact. At 500 styled instances, warm times are
220.952/217.866ms, first renders 299.416/301.943ms and peaks
90,849,176/70,815,888 bytes; exact output is 87,646 bytes. The unchanged complex
1,000-instance attempt exceeded default `max_html_nodes=25000`; its failure is
retained, with no payload removal or limit increase. Six nested captures cover
1/10/100 levels. Depth 100 takes 9.575/9.575ms warm, 72.479/69.718ms first,
and 63,415,280/63,431,872-byte sampled VM peaks, with an exact reference PDF.
First samples are exploratory; 10ms VM sampling excludes native and brief peaks.

Four broad template records move from planned to partial. No template feature is
promoted to full support. Inventory audit retains all 17,267 source records with
no loss or duplication: 2,251 implemented, 4,649 partial, 2,243 planned,
421 printable exclusions and 7,703 non-printing records. Compatibility, examples,
parity notes and changelog document this initial support and its remaining work.


## Batch 31: child and type positional selectors

The parser supports integer An+B expressions for nth-child, nth-last-child,
nth-of-type and nth-last-of-type, plus only-child and only-of-type. Canonical
HtmlValidator grammar handles signs, zero/negative coefficients, odd/even, ASCII
keyword casing and permitted whitespace before numeric conversion. Shared token,
numeric and CSS-work limits remain unchanged. CssParser delegates validation.
Supported compound negations can contain positional functions. Actual elements,
including unused templates, count as siblings; text and comments do not.

Style caches sibling positions within each render, keyed by fresh parent
identities. A changed-tree check proves shared stylesheet caching cannot reuse
another sibling list. Validated compute now prepares stylesheet entries without
repeating full DOM preflight; standalone public stylesheet APIs still validate.
No public trust flag or validation bypass was added. Full selector escapes,
namespace grammar, Level 4 of-selector-list filtering, deeper functional nesting
and parentless/fragment-root semantics remain unfinished.

Five checks extend existing files, with 269 focused tests passing. The browser
fixture retains every label and styles forward/reverse/type/negative/zero/only
positions, including an actual hidden template sibling. Its scripting-disabled
one-page comparison passed at 0.579837% changed pixels, and both full pages were
inspected. Matrix `.quality/logs/20261008T122138Z-1260116/` passed 1,206 tests,
100% coverage, performance, Dialyzer and 111 browser checks on both supported
runtimes, without WARN/FAIL. Exact host comparison also passed all 111.

Twenty sequential captures are combined in
`.quality/performance/results/1.1.0-batch-31/reports.json`. Twelve retain fixed
fee6c4d, previous B30 and candidate inputs, options, assets, CPUs 4,5 and both
revision orders. All 19 input hashes match and previous/candidate output sizes
are unchanged. Whole-process measurements showed a 36.197% large-PNG timing gap
on Elixir 1.20 and table-heading gaps of 1.498%/7.506%. The fresh-process,
31-samples-per-revision comparisons retain these observations and investigate
process history by alternating only the three changed modules. All 21 workloads
retain complete output hashes. Large PNG becomes -0.027%/-0.112% timing and
+0.001%/-0.012% reductions; headings become +0.369%/-0.767% timing and
-1.379%/-1.413% reductions. The largest ordinary fresh timing increases are
+1.189%/+1.395%, with no reproducible B31 slowdown. Affected renderer reductions
fall after duplicate preflight removal. Ordinary Text-field fill is
-0.186%/-0.139% and flatten +0.303%/+0.547%, with exact PDFs. Fixed milestone
performance, including remaining cumulative table cost, stays open.

Six separate selector captures retain 1/100/1,000 labels, sibling spans, unused
templates and denied-looking unused assets. Every first/repeated full PDF equals
its explicit-style reference, with no unused text, field or resolver call.
At 1,000 instances, warm medians are 116.743/106.757ms versus references
112.755/93.277ms. Additional reductions are 177,362/219,429. Exploratory first
renders are 194.064/182.093ms, sampled VM peaks 85,235,104/77,856,464 bytes and
exact output 175,341 bytes. Smaller 1/100 medians are 9.603/24.117ms and
8.652/21.977ms. First samples are exploratory; 10ms VM sampling excludes native
and brief peaks. All captured candidate source hashes match the accepted source.

Two already-partial selector records receive updated notes. Category counts stay
2,251 implemented, 4,649 partial, 2,243 planned, 421 printable exclusions and
7,703 non-printing records. Audit retains all 17,267 IDs without loss/duplication.
Compatibility, examples, parity notes and changelog describe the supported subset.

## Batch 32: script-supporting placement and static SVG input policy

Data scripts and ordinary templates remain actual siblings inside lists,
description lists, supported table structures, selects and buttons. They do not
become terms, rows, cells, columns or choices. Implicit row groups retain both;
column groups retain templates and end before a data script. Canonical select
option views preserve field names, choices, labels and values. Malformed supplied
select trees return actionable style diagnostics before consumers access options.

Visible script/template boxes in CSS table structures diagnose pending anonymous
table formatting. Layout diagnostics now pass through the render API unchanged.
This fixes an ordinary failure that previously interpolated a diagnostic tuple
and raised. Author-styled data and empty template boxes outside these structures
still paint through the existing layout path.

Raw style/title/textarea closing tags use ASCII byte matching after canonical
UTF-8 validation. Unicode-looking closing tags remain literal data. This removes
repeated UTF-8 scans of the remaining document in dense unused template content.
The complete 200-block pilot exceeded the unchanged 25,000-node limit; final
captures retain the full payload at 1/10/100 blocks without dropping cases.

SVG image assets now delegate decoded XML event/script policy to the shared
ForeignContentValidator. Case-variant and namespace-qualified event names,
foreign scripts and executable XHTML scripts diagnose before conversion.
Unqualified XHTML type attributes alone classify HTML data scripts; qualified
XML attributes do not impersonate HTML attributes. SAX policy reads local names
without converting attribute values a second time. Supplied inactive DOM event
attributes use the same ASCII rule. No inline SVG painting is enabled.

Eight focused checks extend existing test files. The final isolated optimized
variant passed 326 focused tests. The scripting-disabled one-page fixture passed
at 0.824592% changed pixels, with all labels and complete pages inspected. Initial
default select styling showed 1.503497%; supported explicit dimensions keep every
case and pass without changing tolerances. Default native control geometry and
arrow painting remain unfinished. Full HTML insertion modes, anonymous table
formatting, new select modes and SVG composition remain open.

Matrix `.quality/logs/20261008T155138Z-1536417/` passed 1,214 tests, 100% coverage,
performance, Dialyzer and 112 browser comparisons on both supported runtimes,
without WARN/FAIL. Exact host comparison also passed all 112.

Thirty-six sequential captures are combined in
`.quality/performance/results/1.1.0-batch-32/reports.json`. Twelve preserve fixed
fee6c4d, previous B31 and candidate inputs/options/assets on CPUs 4,5 in both
revision orders. Twelve separately retain complete 1/10/100-block static and
interactive feature payloads. Two fresh-process comparisons alternate 31 samples
per revision for all 19 original operations and ordinary Text-field fill/flatten.
Six compare existing SVG validation and complete PNG conversion at 1/100/1,000
shapes. Four targeted follow-ups use 61 samples per revision. All captured
candidate source hashes match the accepted source. Earlier failed/stale-source
captures and slower raw-text/SAX variants remain excluded, with complete evidence
under `.quality/performance/excluded-captures/`.

Whole-process full-font timing gaps of +3.015%/+4.596% shrink in fresh processes
to +0.818%/+1.401%. Fresh headings become +0.310%/-2.692% timing and
+0.351%/+0.182% reductions. Text-field fill is -0.106%/+0.241%, flatten
+1.222%/-0.639%. Every one of the 21 workloads retains identical complete output
hashes. The targeted follow-up results are retained below, in runtime order.

Elixir 1.19, 100-row uncompressed full-font render: 135.768ms versus 136.079ms, -0.229% timing and +0.071% reductions.

Elixir 1.19, 160 table headings: 55.762ms versus 55.233ms, +0.958% timing and +0.326% reductions.

Elixir 1.19, 100 ordinary Text-field flatten: 69.043ms versus 69.487ms, -0.639% timing and +0.003% reductions.

Elixir 1.20, 100-row uncompressed full-font render: 128.877ms versus 128.441ms, +0.339% timing and +0.433% reductions.

Elixir 1.20, 160 table headings: 55.135ms versus 55.490ms, -0.640% timing and +0.344% reductions.

Elixir 1.20, 100 ordinary Text-field flatten: 67.390ms versus 67.166ms, +0.334% timing and -0.019% reductions.

Small ordinary reduction costs come from canonical specialized-parent views and
parent-aware table layout validation. The fixed milestone baseline and cumulative
cost remain visible; passing guardrails does not complete milestone performance.

The initial SVG policy mapped every decoded attribute value twice. The accepted
variant reads SAX local names directly, preserves original values and converts
unqualified attributes once. At 1,000 shapes, first optimized validation medians
are 7.174/7.153ms versus 6.995/6.928ms. This removes most of the previous extra
1.328/1.144ms and 267,726/254,825 reductions. Remaining event-name scanning is
required to reject executable attributes after XML decoding, including qualified
and case-variant names. A raw source search cannot provide that guarantee. No
SVG script or event check was removed to improve performance. The 61-sample
follow-up keeps the 86,144-byte source, every shape and the exact PNG:

Elixir 1.19, SVG validate: 7.984ms versus 7.822ms, extra +0.162ms, +2.071% timing and +48,377 reductions.

Elixir 1.19, SVG rasterize: 9.716ms versus 9.515ms, extra +0.201ms, +2.112% timing and +49,101 reductions.

Elixir 1.20, SVG validate: 7.149ms versus 6.835ms, extra +0.314ms, +4.594% timing and +45,670 reductions.

Elixir 1.20, SVG rasterize: 8.817ms versus 8.644ms, extra +0.173ms, +2.001% timing and +39,223 reductions.

Complete placement payloads at 100 blocks retain 800 scripts, 800 templates,
all active choices and all unused attributes/content. Static warm medians are
149.488/140.613ms versus 82.208/76.272ms references; interactive medians are
224.888/216.198ms versus 121.955/114.766ms. First renders are exploratory, and
10ms sampled VM memory excludes native/brief peaks. Static outputs are 98,397
bytes and interactive outputs 356,561 bytes, exactly equal to full references.
Before ASCII boundary matching, static medians were 282.558/286.327ms and
interactive 364.359/351.618ms. Unused sources never resolve, every first/repeated
PDF matches and flattened appearances retain all visible labels and field values.

Thirteen already-partial inventory records receive updated notes. Counts stay
2,251 implemented, 4,649 partial, 2,243 planned, 421 printable exclusions and
7,703 non-printing records. Audit preserves all 17,267 IDs without duplication.
Compatibility, examples, parity notes and changelog describe the delivered subset.

## Batch 33: initial scripting-disabled noscript

The native static document model parses noscript children as active HTML. Supported
inline fallback and author display:block fallback retain text, descendants,
selectors, controls and resources. Head fallback accepts supported style/meta
children; its styles apply to the document and metadata preserves source order
and caller precedence. Nested actual noscript descendants and invalid transparent
or head contents diagnose through canonical HtmlValidator rules. Template fragments
are separate trees: unused fallback stays parsed but cannot collect styles,
metadata, fields or assets. Active fallback images follow the same authorized
asset resolver and denial diagnostics as ordinary images.

Contextual checks run for the new feature. Ordinary element validation keeps its
existing three-argument entry point. Direct DOM callers receive the same semantic
rules. Default inline noscript wrapping block children still needs anonymous block
formatting; it returns an actionable layout diagnostic recommending supported
inline content or explicit display:block. HTML-discovered link processing remains excluded. Full insertion/select
contexts and decorated inline composition remain unfinished.

Eight checks extend existing parser/style/render/validator files. The final
isolated parser/style/render/layout suite passed 470 tests before the last
validator-compatibility check. The first matrix passed tests but left untested
compatibility call forms and malformed fragment content, yielding 99.98% coverage.
The added focused check restored 100% without changing source or thresholds.
Final matrix `.quality/logs/20261008T163116Z-1620296/` passed 1,222 tests, coverage,
performance, Dialyzer and 113 browser comparisons on both runtimes, without
WARN/FAIL. Exact host comparison also passed all 113.

The one-page scripting-disabled fixture passes at 0.213068% changed pixels, with
all labels/fields retained and complete native/browser pages inspected. An initial
10pt/13pt fractional control style produced 1.281566%; its full pages are retained
in `.quality/batch-33-noscript-preview-first-pixel-failure`. Of 812 changed pixels,
706 were in the control region. The final fixture uses the explicit 10.5pt/13.5pt,
1.5pt-padding style already used by accepted input fixtures, preserving every
fallback case. Default control geometry and the original fractional widget case
remain open. Browser tolerances are unchanged.

Twenty-eight sequential captures are consolidated under ignored
`.quality/performance/results/1.1.0-batch-33/reports.json`. Twelve preserve fixed
fee6c4d, previous B32 and candidate ordinary inputs/options/assets on CPUs 4,5
in both revision orders. Twelve separately preserve full 1/10/100-instance static
and interactive fallback payloads. Two fresh-process comparisons alternate
31 samples per revision for all 19 original operations and ordinary Text-field
fill/flatten. Every complete output hash matches, and candidate source hashes
match the accepted source. Two targeted follow-ups retain 61 samples per revision
for invoices, purchase orders and full-font rendering. Fixed milestone performance
remains open.

Elixir 1.19, 100-row uncompressed full-font render: 138.109ms versus 136.957ms, +0.841% timing and +0.004% reductions.

Elixir 1.19, 160 table headings: 56.381ms versus 56.059ms, +0.574% timing and -0.005% reductions.

Elixir 1.19, 100 ordinary Text-field fill: 398.560ms versus 396.053ms, +0.633% timing and -0.001% reductions.

Elixir 1.19, 100 ordinary Text-field flatten: 72.635ms versus 72.119ms, +0.715% timing and +0.007% reductions.

Elixir 1.20, 100-row uncompressed full-font render: 127.904ms versus 128.241ms, -0.263% timing and -0.003% reductions.

Elixir 1.20, 160 table headings: 54.448ms versus 55.087ms, -1.160% timing and +0.023% reductions.

Elixir 1.20, 100 ordinary Text-field fill: 386.281ms versus 387.149ms, -0.224% timing and +0.000% reductions.

Elixir 1.20, 100 ordinary Text-field flatten: 67.917ms versus 67.786ms, +0.193% timing and +0.019% reductions.

Elixir 1.19, 61-sample follow-up, fixture invoice_012.html: 27.325ms versus 27.201ms, +0.456% timing and -0.005% reductions.

Elixir 1.19, 61-sample follow-up, fixture purchase_order.html: 41.409ms versus 40.605ms, +1.980% timing and +0.072% reductions.

Elixir 1.19, 61-sample follow-up, 100-row uncompressed full-font render: 135.364ms versus 135.448ms, -0.062% timing and +0.003% reductions.

Elixir 1.20, 61-sample follow-up, fixture invoice_012.html: 26.676ms versus 26.503ms, +0.653% timing and +0.030% reductions.

Elixir 1.20, 61-sample follow-up, fixture purchase_order.html: 41.480ms versus 41.818ms, -0.808% timing and +0.009% reductions.

Elixir 1.20, 61-sample follow-up, 100-row uncompressed full-font render: 128.403ms versus 127.636ms, +0.601% timing and +0.003% reductions.
'
The Elixir 1.19 purchase order was 2.161% slower in the initial fresh-process
comparison and 1.980% slower in the targeted follow-up. Both results remain in the
acceptance bundle. Eprof recorded unchanged ordinary parser, validator, style and
resource-check call counts. The new metadata helper ran ten times across ten
renders, taking 1us under instrumentation. Instrumented timing differences also
occurred across unchanged font, regex and collection functions; profiling alone
cannot establish their cause.

A separate factorial diagnostic alternated the compatible parser/foreign-validator
pair, Style and HtmlValidator across all eight combinations, 31 fresh samples per
combination on CPUs 4,5. Complete purchase PDFs matched for every combination.
The full candidate median was 41.918ms versus 42.053ms for previous source, -0.321%
time and -0.011% reductions. All combination medians were 41.653 to 42.126ms;
no changed group reproduced the earlier 2% penalty. The earlier timing gap is
therefore not stable across the controlled comparisons, and this batch adds no
extra feature traversal to ordinary documents. These measurements do not prove
that all smaller costs are absent. Continue fixed-baseline monitoring.

The raw attribution report and SHA manifest are separate diagnostic evidence in
`.quality/performance/diagnostic-captures/1.1.0-batch-33/`. Hybrid module combinations
are not accepted source revisions; that report records no library source hashes.
The proposed validation-accumulator optimization passed isolated checks but showed
no improvement in an excluded pilot, and was left unmerged. Original B33 source
passes all final gates and its hashes match all candidate acceptance captures.

At 100 complete instances, static warm medians are 73.690/62.645ms versus
72.697/61.640ms references. Interactive medians are 105.208/101.972ms versus
103.728/102.937ms. Exact PDFs are 164,581/411,411 bytes, and all 100 active fields
retain initial values. Unused duplicate names/styles/images never activate.
First samples are exploratory; 10ms VM sampling excludes native/brief peaks.

Four records move from planned to partial. Counts become 2,251 implemented,
4,653 partial, 2,239 planned, 421 printable exclusions and 7,703 non-printing
records. Audit preserves all 17,267 IDs. Initial fallback is delivered; complete
static composition, native geometry, SVG/MathML and embedded documents remain open.

## Batch 34: initial anonymous block formatting

Default inline noscript and neutral inline ancestors can now contain supported
in-flow blocks. Text before and after a nested block remains in its surrounding
inline group. Nested block boxes retain their own style and percentage width;
sibling margins, table-cell flow and atomic inline-block contents use the ordinary
layout paths. The original DOM and selector ancestry remain intact. Editable
controls and unused template resource policy remain unchanged.

The canonical layout validator detects block descendants during its existing walk.
It propagates that result through inline ancestors and checks fragment eligibility
only for ancestors interrupted by a block. Backgrounds, box edges, links, relative
positioning, clipping, vertical alignment and attached positioned children require
remaining fragment behavior and diagnose at the responsible ancestor. Replaced
images within inline ancestors diagnose instead of discarding the ancestor's
painting. Unsupported nested flex content still returns its existing failure.

Grouping combines inline siblings and promotes validated nested blocks during the
same recursive pass. The prototype's additional full-list normalization and
consumer-side eligibility checks were removed. It preserves ordinary inline
wrappers when their children remain inline; it keeps atomic and hidden boxes
intact. No tunable limits, trust flags, source rewriting or cache permissions were
added.

Ten checks extend existing render, layout and validator files. Full reference
PDF checks retain text/run boundaries, active fields, empty fragments, nesting,
percentage widths, positive/negative sibling margins, table cells and atomic
containers. Direct malformed trees and unsupported ancestor properties diagnose.
The first matrix passed all functional tests but reported 99.99% coverage. Empty
inline groups and the existing nested-flex failure path received focused coverage.
The pre-margin matrix `.quality/logs/20261008T172204Z-1708207/` passed 1,230 tests, 100%
coverage, performance, Dialyzer and 114 browser comparisons on both runtimes,
without FAIL/WARN.

The pre-margin one-page scripting-disabled fixture passed at 0.468224% changed
pixels. Both PDFs retained every label, including half-width table-cell content
and atomic inline flow; both full PNGs were inspected. Tolerances are unchanged.
CSS 2.2 anonymous block rules are the primary source for this layout behavior:
https://www.w3.org/TR/CSS22/visuren.html#anonymous-block-level

This delivers the initial undecorated flow. Complete decorated/positioned/link
fragments, replaced-inline images, anonymous tables and intrinsic sizing remain
unfinished. HTML-discovered link processing remains excluded by the approved
profile; explicit stylesheets and inline style remain supported. Complete static
HTML, SVG, MathML and isolated embedded documents are still milestone work.

A boundary probe exposed incorrect parent-child margin collapse through inline
ancestors. Without padding, painted parent boxes differed from their direct-block
references; with padding, all original probes matched. The correction follows
eligible first/last content paths during the existing margin-collapse pass. It
preserves collapse barriers, out-of-flow siblings, preformatted whitespace and
non-breaking spaces. It updates the actual nested block margin without rewriting
selector ancestry. Direct child updates retain the original List.replace_at path;
deeper updates rebuild only the resolved path. Twelve original complete-PDF probes
and 48 empty/nonempty, positive/negative, padded/bordered boundary cases match.
The final focused suite passes 313 tests. Two new checks extend existing files.

The 32 pre-margin captures, complete original raw bytes and SHA checks are retained
separately at `.quality/performance/excluded-captures/1.1.0-batch-34-before-margin-collapse/`.
They are not final acceptance evidence. The standalone ordinary benchmark initially
retained two unused aliases after narrowing the compared modules; those project
warnings were removed before corrected captures. Final renderer matrix logs remain
subject to the repository's warning-free gate.

Profiling the margin correction found 160 extra trim calls per repeated-text
render. The new scan only needs a whitespace predicate, so it now uses a fully
anchored ASCII-whitespace match. Nonbreaking and preserved whitespace remain
content. A separate excluded three-way pilot on CPUs 0,1 retained identical
complete PDFs. Elixir 1.20 medians were 26.206ms and 3,070,653 reductions for B33,
26.351ms and 3,082,611 for the unoptimized correction, and 26.330ms and 3,073,894
for the boolean check. Instrumented calls establish the removed work; those pilot
timings are not acceptance measurements.

The 34 corrected captures before this optimization, including the five-workload
61-sample follow-up, retain all original bytes, SHA checks, logs and exact library
source under `.quality/performance/excluded-captures/1.1.0-batch-34-before-whitespace-optimization/`.
The follow-up did not reproduce the larger whole-run purchase/paragraph gaps.
They remain recorded rather than being overwritten by the final variant.

Final optimized source passes `.quality/logs/20261008T220829Z-1956761/` on both
supported runtimes with 1,232 tests, 100% coverage, performance, Dialyzer and 114
browser comparisons. No FAIL or WARN remains. Exact host parity also passes 114
comparisons. The complete one-page final fixture remains at 0.441632% changed
pixels, with all labels and parent margin boundaries retained. Both full pages
and extracted text were inspected. Thresholds are unchanged.

The final 31-sample Elixir 1.20 sticker timing was +4.489% despite slightly lower
reductions. Earlier whole-run timing was +0.783% and earlier corrected fresh
measurement was -0.181%. All original reports remain retained. A final six-workload
61-sample follow-up includes this outlier and repeated text in both runtimes.
Sticker profiling retains equal complete PDFs and unchanged validator/tree counts,
regex calls and whitespace trims. The new margin scan runs once per render;
under ten-render instrumentation its helper and callback total 3us. Grouping uses
its existing pass. No full additional traversal or resource read was added.
Instrumentation cannot by itself establish why the earlier timing differed.

The final 61-sample sticker follow-up retained a +2.396% timing gap on 1.20,
+0.034% reductions. A separate controlled factorial diagnostic then alternated
Layout and HtmlValidator independently across four ordinary-input combinations,
61 fresh-process samples per combination on CPUs 4,5. Every complete PDF matched.
Full candidate was 11.591ms versus 11.554ms, +0.320% time and +0.005% reductions;
all four medians were 11.525 to 11.591ms. Neither module reproduced the earlier
2-4.5% penalty. The larger gap is therefore not stable across these comparisons.
The small extra boundary lookup is required for correct margin collapse and runs
during the existing pass; avoidable trimming and additional grouping traversals
have been removed. These results do not establish that every smaller cost is absent.
All earlier outliers remain in the bundle. Diagnostic source/BEAM hashes and raw
samples are retained separately at
`.quality/performance/diagnostic-captures/1.1.0-batch-34/`, with a SHA manifest.
Hybrids are diagnostic combinations and are not accepted source revisions.

34 final captures are consolidated at
`.quality/performance/results/1.1.0-batch-34/reports.json`. Twelve preserve the
fixed fee6c4d baseline, previous B33 and candidate ordinary workloads on CPUs 4,5
in both revision orders. Twelve measure complete 1/10/100-instance static and
interactive anonymous fallback. Six measure nesting depths 1/10/50. Two alternate
31 fresh-process samples per revision for all 19 original operations and ordinary
Text-field fill/flatten. All 21 complete ordinary outputs retain identical hashes.
Every candidate library hash matches the accepted source. Fixed milestone
performance remains open; no baseline or guardrail changed.

Elixir 1.19, fresh-process fixture invoice_012.html: 27.952ms versus 27.660ms, +1.056% time and -0.014% reductions.

Elixir 1.19, fresh-process fixture statement_012.html: 27.295ms versus 27.065ms, +0.850% time and +0.002% reductions.

Elixir 1.19, fresh-process fixture multi_page_report_012.html: 29.357ms versus 29.541ms, -0.623% time and -0.048% reductions.

Elixir 1.19, fresh-process fixture purchase_order.html: 42.654ms versus 42.469ms, +0.436% time and -0.032% reductions.

Elixir 1.19, fresh-process fixture material_requisition.html: 57.201ms versus 57.352ms, -0.263% time and -0.023% reductions.

Elixir 1.19, fresh-process fixture government_application_form.html: 41.454ms versus 41.433ms, +0.051% time and -0.039% reductions.

Elixir 1.19, fresh-process fixture stock_sticker.html: 12.240ms versus 12.294ms, -0.439% time and -0.002% reductions.

Elixir 1.19, fresh-process fixture trim_card.html: 50.314ms versus 51.703ms, -2.686% time and -0.205% reductions.

Elixir 1.19, fresh-process 100-row HTML render: 155.254ms versus 154.206ms, +0.680% time and -0.027% reductions.

Elixir 1.19, fresh-process 100-row uncompressed full-font render: 136.662ms versus 137.547ms, -0.643% time and -0.071% reductions.

Elixir 1.19, fresh-process 100-row PDF text extraction: 68.309ms versus 68.436ms, -0.186% time and +0.020% reductions.

Elixir 1.19, fresh-process 100-row PDF visual text extraction: 68.464ms versus 69.138ms, -0.975% time and -0.011% reductions.

Elixir 1.19, fresh-process repeated text layout: 25.331ms versus 25.377ms, -0.181% time and -0.009% reductions.

Elixir 1.19, fresh-process long paragraph layout: 50.107ms versus 50.257ms, -0.298% time and +0.027% reductions.

Elixir 1.19, fresh-process five repeated PNG images: 13.493ms versus 13.518ms, -0.185% time and +0.003% reductions.

Elixir 1.19, fresh-process ten repeated 512px PNG images: 330.928ms versus 329.073ms, +0.564% time and -0.003% reductions.

Elixir 1.19, fresh-process 800-word inline paragraph: 41.264ms versus 42.441ms, -2.773% time and -0.108% reductions.

Elixir 1.19, fresh-process 160 table headings: 55.424ms versus 55.335ms, +0.161% time and -0.064% reductions.

Elixir 1.19, fresh-process 4,000 boxes on one page: 14.496ms versus 14.515ms, -0.131% time and +0.012% reductions.

Elixir 1.19, fresh-process 100 ordinary Text-field fill: 396.341ms versus 396.845ms, -0.127% time and -0.007% reductions.

Elixir 1.19, fresh-process 100 ordinary Text-field flatten: 69.948ms versus 69.076ms, +1.262% time and +0.010% reductions.

Elixir 1.19, 100 complete static fallback instances: 44.761ms versus 47.116ms explicit references; +122,774 additional reductions. First render 114.827ms, sampled peak VM memory 69,562,160 bytes and complete output 87,017 bytes.

Elixir 1.19, 100 complete interactive fallback instances: 84.476ms versus 84.432ms explicit references; +133,397 additional reductions. First render 160.248ms, sampled peak VM memory 79,678,904 bytes and complete output 332,853 bytes.

Elixir 1.20, fresh-process fixture invoice_012.html: 26.465ms versus 26.588ms, -0.463% time and -0.043% reductions.

Elixir 1.20, fresh-process fixture statement_012.html: 26.015ms versus 25.923ms, +0.355% time and -0.034% reductions.

Elixir 1.20, fresh-process fixture multi_page_report_012.html: 28.274ms versus 28.172ms, +0.362% time and -0.011% reductions.

Elixir 1.20, fresh-process fixture purchase_order.html: 41.048ms versus 41.173ms, -0.304% time and -0.031% reductions.

Elixir 1.20, fresh-process fixture material_requisition.html: 56.107ms versus 56.261ms, -0.274% time and -0.001% reductions.

Elixir 1.20, fresh-process fixture government_application_form.html: 40.121ms versus 39.761ms, +0.905% time and -0.011% reductions.

Elixir 1.20, fresh-process fixture stock_sticker.html: 12.384ms versus 11.852ms, +4.489% time and -0.036% reductions.

Elixir 1.20, fresh-process fixture trim_card.html: 50.109ms versus 50.173ms, -0.128% time and -0.206% reductions.

Elixir 1.20, fresh-process 100-row HTML render: 140.935ms versus 141.788ms, -0.602% time and -0.107% reductions.

Elixir 1.20, fresh-process 100-row uncompressed full-font render: 129.678ms versus 128.956ms, +0.560% time and -0.060% reductions.

Elixir 1.20, fresh-process 100-row PDF text extraction: 64.964ms versus 64.856ms, +0.167% time and -0.008% reductions.

Elixir 1.20, fresh-process 100-row PDF visual text extraction: 66.504ms versus 66.306ms, +0.299% time and +0.000% reductions.

Elixir 1.20, fresh-process repeated text layout: 26.250ms versus 26.115ms, +0.517% time and +0.111% reductions.

Elixir 1.20, fresh-process long paragraph layout: 44.019ms versus 43.912ms, +0.244% time and +0.042% reductions.

Elixir 1.20, fresh-process five repeated PNG images: 13.556ms versus 13.520ms, +0.266% time and -0.004% reductions.

Elixir 1.20, fresh-process ten repeated 512px PNG images: 326.468ms versus 326.236ms, +0.071% time and +0.025% reductions.

Elixir 1.20, fresh-process 800-word inline paragraph: 40.613ms versus 42.951ms, -5.443% time and -0.216% reductions.

Elixir 1.20, fresh-process 160 table headings: 55.067ms versus 55.398ms, -0.597% time and +0.008% reductions.

Elixir 1.20, fresh-process 4,000 boxes on one page: 12.415ms versus 12.387ms, +0.226% time and -0.033% reductions.

Elixir 1.20, fresh-process 100 ordinary Text-field fill: 392.989ms versus 392.956ms, +0.008% time and +0.003% reductions.

Elixir 1.20, fresh-process 100 ordinary Text-field flatten: 68.137ms versus 69.589ms, -2.087% time and +0.004% reductions.

Elixir 1.20, 100 complete static fallback instances: 43.455ms versus 44.204ms explicit references; +126,270 additional reductions. First render 114.473ms, sampled peak VM memory 78,215,584 bytes and complete output 87,017 bytes.

Elixir 1.20, 100 complete interactive fallback instances: 83.129ms versus 82.618ms explicit references; +126,052 additional reductions. First render 159.922ms, sampled peak VM memory 79,185,592 bytes and complete output 332,853 bytes.

Two final targeted follow-ups retain 61 fresh-process samples per revision for invoice, purchase, sticker, repeated-text, full-font and inline-paragraph workloads.

Elixir 1.19, 61-sample fixture invoice_012.html: 27.502ms versus 27.395ms, +0.391% time and -0.004% reductions.

Elixir 1.19, 61-sample fixture purchase_order.html: 41.290ms versus 41.378ms, -0.213% time and -0.038% reductions.

Elixir 1.19, 61-sample fixture stock_sticker.html: 12.260ms versus 12.261ms, -0.008% time and -0.031% reductions.

Elixir 1.19, 61-sample 100-row uncompressed full-font render: 137.459ms versus 136.162ms, +0.953% time and -0.015% reductions.

Elixir 1.19, 61-sample repeated text layout: 26.174ms versus 27.319ms, -4.191% time and -0.138% reductions.

Elixir 1.19, 61-sample 800-word inline paragraph: 43.014ms versus 42.900ms, +0.266% time and +0.005% reductions.

Elixir 1.20, 61-sample fixture invoice_012.html: 26.754ms versus 26.765ms, -0.041% time and -0.011% reductions.

Elixir 1.20, 61-sample fixture purchase_order.html: 41.782ms versus 41.513ms, +0.648% time and -0.063% reductions.

Elixir 1.20, 61-sample fixture stock_sticker.html: 12.391ms versus 12.101ms, +2.396% time and +0.034% reductions.

Elixir 1.20, 61-sample 100-row uncompressed full-font render: 129.593ms versus 129.412ms, +0.140% time and -0.046% reductions.

Elixir 1.20, 61-sample repeated text layout: 25.192ms versus 25.389ms, -0.776% time and +0.103% reductions.

Elixir 1.20, 61-sample 800-word inline paragraph: 40.211ms versus 40.332ms, -0.300% time and +0.017% reductions.

First feature samples are exploratory. VM sampling runs every 10ms and does not
measure native or brief allocation peaks. Full fallback text, all editable names
and initial values, and inactive templates/resources remain in every comparison.
No payloads were trimmed to fit limits. Paired whole-run timing variation and
its retained earlier targeted checks must be read with the final fresh-process
comparisons; no timing threshold grants a regression budget.

Five existing partial inventory records receive updated notes. Categories remain
2,251 implemented, 4,653 partial, 2,239 planned, 421 printable exclusions and
7,703 non-printing records. The audit retains all 17,267 IDs. Anonymous formatting
is still partial; complete static HTML, SVG, MathML and child-document rendering
remain milestone work.

## Batch 35: shared CSS identifiers and escapes

Unicode and escaped type/class/id names, attribute names and values, and supported
pseudo names now use shared validator-owned CSS identifier grammar. Selector and
rule splitting preserve escaped punctuation and hexadecimal whitespace boundaries.
At-rule preprocessing keeps quoted strings and escaped at signs opaque. Continued
attribute strings keep literal at-rule-looking content. Invalid scalar escapes
become U+FFFD. Type and attribute names fold ASCII only; class and id values keep
Unicode and case. HTML class lists split only ASCII whitespace, retaining nonbreaking
names and combining characters. Attribute case flags share the same escape decoding.

CssParser delegates grammar and keyword validation to HtmlValidator. Style uses
canonical class tokens. Shared cleanup remains reused; unused ASCII source paths avoid escape decoding.
Escape-unit recognition uses a direct binary match, avoiding per-character
String.starts_with? calls. The six-digit bound is a fixed CSS grammar invariant.
No tunable limits, resource permissions, trust flags or output shortcuts changed.

Seven new checks extend four existing test files. They cover exact complete-PDF
reference output, decoded names and specificity, escape terminators, literal
punctuation/quotes/at-rule text, Unicode case, combining/nonbreaking names, string
continuations, scalar replacements, pseudo names and actionable public diagnostics.
The full staged run passed 1,239 tests at 100% coverage. Final matrix
`.quality/logs/20261008T235330Z-2217392/` passes both supported runtimes with
1,239 tests, 100% coverage, performance, Dialyzer and 115 browser comparisons.
Every stage is warning-free. Required exact host parity passes all 115 cases.
Controlled captures and the shared-context table profile are verified before
acceptance.

The one-page scripting-disabled fixture passes pinned Chromium at 0.366162%
changed pixels. Both complete pages and all eight labels, generated prefix,
colors and attribute background were inspected. Native and Chromium extracted
text match. Per-page tolerances remain unchanged.

Primary CSS grammar source: https://www.w3.org/TR/css-syntax-3/ . The initial
ordinary staging pilot preserves 21 exact outputs and its verified source files
in ignored diagnostic storage. Its changed compressed-table reduction median
is assessed by the final source comparisons and shared-context profile in this
record. Staging measurements are excluded from acceptance. Full CSS preprocessing, namespaces, functional-selector lists and
remaining included grammar are still milestone work. This supplies the selector
dependency for unregistered custom containers; custom DOM rendering is not enabled
by this batch. Full static HTML, SVG, MathML and child documents remain unfinished.


The 61-sample follow-ups do not reproduce the earlier government-form or
table-heading timing gaps. Purchase timing reverses direction between captures:
1.19 fresh +6.799% becomes -4.328%; 1.20 fresh -7.449% becomes +2.835%. Reductions
are -0.377% and +0.003% in the longer purchase trials. No stable purchase slowdown
is established. Five-image reductions are -0.002% and -0.004% in the longer trials;
the rejected empty-class variant's increase is removed.

Compressed-table reductions increase by 5,699 on 1.19, 0.044688%, and 735 on 1.20,
0.006406%, in the longer trials. Corresponding time changes are +0.555% and -0.151%;
the original fresh trials are -0.871% and -0.962%. Shared-application profiling
retains all font, image, writer and layout call counts. It finds 1,216 canonical
class-token delegations and 20 additional regular-expression operations per
render, while replacing 398 grapheme/Unicode iterator calls with codepoint work.
These are required shared ASCII class/identifier semantics, with no added tree
traversal or repeated resource work. Profiling does not isolate each function's
share of the small reduction delta, and instrumented timings are excluded.
The faster empty-class branch was tried and rejected because it increased image
reductions. The parser's avoidable per-character prefix calls were removed. This
small remaining correctness cost is recorded explicitly; timing has no stable
slowdown beyond the retained variation. Guardrails are not a regression budget.


34 final captures are consolidated at
`.quality/performance/results/1.1.0-batch-35/reports.json`. Twelve compare the fixed
fee6c4d baseline, previous B34 and candidate in both revision orders on CPUs 4,5.
Twelve retain complete 1/10/100-instance static and interactive selector workloads.
Six retain nesting depths 1/10/50. Two alternate 31 fresh-process samples per
revision for all 19 original operations and ordinary Text-field fill/flatten.
All 21 complete ordinary outputs retain identical hashes. All candidate hashes
match all 78 accepted library sources. Guardrails and the fixed baseline remain
unchanged. Fixed cumulative milestone performance remains open.

Elixir 1.19, fixture invoice_012.html: 28.220ms versus 27.673ms, +1.977% time and -0.135% reductions.

Elixir 1.19, fixture statement_012.html: 27.307ms versus 27.540ms, -0.846% time and -0.178% reductions.

Elixir 1.19, fixture multi_page_report_012.html: 29.950ms versus 30.088ms, -0.459% time and -0.174% reductions.

Elixir 1.19, fixture purchase_order.html: 43.985ms versus 41.185ms, +6.799% time and -0.028% reductions.

Elixir 1.19, fixture material_requisition.html: 57.662ms versus 59.409ms, -2.941% time and -0.291% reductions.

Elixir 1.19, fixture government_application_form.html: 41.867ms versus 42.456ms, -1.387% time and -0.243% reductions.

Elixir 1.19, fixture stock_sticker.html: 12.499ms versus 12.692ms, -1.521% time and -0.332% reductions.

Elixir 1.19, fixture trim_card.html: 52.429ms versus 53.988ms, -2.888% time and -0.648% reductions.

Elixir 1.19, 100-row HTML render: 149.005ms versus 150.314ms, -0.871% time and +0.034% reductions.

Elixir 1.19, 100-row uncompressed full-font render: 138.184ms versus 138.818ms, -0.457% time and +0.031% reductions.

Elixir 1.19, 100-row PDF text extraction: 69.028ms versus 69.536ms, -0.731% time and -0.017% reductions.

Elixir 1.19, 100-row PDF visual text extraction: 70.596ms versus 70.397ms, +0.283% time and -0.001% reductions.

Elixir 1.19, repeated text layout: 26.174ms versus 26.131ms, +0.165% time and +0.001% reductions.

Elixir 1.19, long paragraph layout: 50.996ms versus 50.804ms, +0.378% time and -0.001% reductions.

Elixir 1.19, five repeated PNG images: 13.754ms versus 13.661ms, +0.681% time and -0.000% reductions.

Elixir 1.19, ten repeated 512px PNG images: 339.709ms versus 340.891ms, -0.347% time and -0.029% reductions.

Elixir 1.19, 800-word inline paragraph: 41.655ms versus 41.287ms, +0.891% time and +0.010% reductions.

Elixir 1.19, 160 table headings: 55.462ms versus 55.725ms, -0.472% time and +0.012% reductions.

Elixir 1.19, 4,000 boxes on one page: 15.428ms versus 15.432ms, -0.026% time and +0.004% reductions.

Elixir 1.19, 100 ordinary Text-field fill: 439.625ms versus 433.662ms, +1.375% time and -0.001% reductions.

Elixir 1.19, 100 ordinary Text-field flatten: 71.657ms versus 71.430ms, +0.318% time and -0.009% reductions.

Elixir 1.19, 100 complete static instances: 56.657ms versus 57.279ms decoded references; -9,766 reductions. First render 144.060ms, sampled VM peak 77,126,760 bytes and output 89,073 bytes.

Elixir 1.19, 100 complete interactive instances: 96.145ms versus 96.059ms decoded references; +25,315 reductions. First render 175.731ms, sampled VM peak 79,664,856 bytes and output 338,144 bytes.

Elixir 1.20, fixture invoice_012.html: 27.044ms versus 27.315ms, -0.992% time and -0.247% reductions.

Elixir 1.20, fixture statement_012.html: 26.135ms versus 26.357ms, -0.842% time and -0.184% reductions.

Elixir 1.20, fixture multi_page_report_012.html: 28.886ms versus 28.986ms, -0.345% time and -0.221% reductions.

Elixir 1.20, fixture purchase_order.html: 38.888ms versus 42.018ms, -7.449% time and -0.512% reductions.

Elixir 1.20, fixture material_requisition.html: 56.176ms versus 56.802ms, -1.102% time and -0.194% reductions.

Elixir 1.20, fixture government_application_form.html: 40.586ms versus 40.651ms, -0.160% time and -0.224% reductions.

Elixir 1.20, fixture stock_sticker.html: 11.735ms versus 11.783ms, -0.407% time and -0.313% reductions.

Elixir 1.20, fixture trim_card.html: 50.155ms versus 51.071ms, -1.794% time and -0.450% reductions.

Elixir 1.20, 100-row HTML render: 142.297ms versus 143.679ms, -0.962% time and +0.006% reductions.

Elixir 1.20, 100-row uncompressed full-font render: 133.858ms versus 132.318ms, +1.164% time and +0.016% reductions.

Elixir 1.20, 100-row PDF text extraction: 68.731ms versus 68.834ms, -0.150% time and -0.006% reductions.

Elixir 1.20, 100-row PDF visual text extraction: 71.147ms versus 72.871ms, -2.366% time and -0.040% reductions.

Elixir 1.20, repeated text layout: 26.137ms versus 26.198ms, -0.233% time and -0.004% reductions.

Elixir 1.20, long paragraph layout: 44.991ms versus 44.978ms, +0.029% time and +0.003% reductions.

Elixir 1.20, five repeated PNG images: 14.059ms versus 13.979ms, +0.572% time and -0.004% reductions.

Elixir 1.20, ten repeated 512px PNG images: 340.390ms versus 340.879ms, -0.143% time and -0.001% reductions.

Elixir 1.20, 800-word inline paragraph: 40.961ms versus 41.373ms, -0.996% time and -0.031% reductions.

Elixir 1.20, 160 table headings: 56.840ms versus 57.052ms, -0.372% time and +0.014% reductions.

Elixir 1.20, 4,000 boxes on one page: 12.622ms versus 12.600ms, +0.175% time and -0.005% reductions.

Elixir 1.20, 100 ordinary Text-field fill: 432.416ms versus 430.468ms, +0.453% time and +0.024% reductions.

Elixir 1.20, 100 ordinary Text-field flatten: 69.278ms versus 70.263ms, -1.402% time and -0.002% reductions.

Elixir 1.20, 100 complete static instances: 55.149ms versus 54.562ms decoded references; -2,968 reductions. First render 127.201ms, sampled VM peak 77,452,080 bytes and output 89,073 bytes.

Elixir 1.20, 100 complete interactive instances: 94.082ms versus 93.689ms decoded references; -2,559 reductions. First render 167.093ms, sampled VM peak 85,313,408 bytes and output 338,144 bytes.

Two targeted follow-ups retain 61 fresh-process samples per revision for ten ordinary operations, including purchase, government forms and table headings.

Elixir 1.19, 61-sample fixture invoice_012.html: 27.231ms versus 27.454ms, -0.812% time and -0.234% reductions.

Elixir 1.19, 61-sample fixture multi_page_report_012.html: 29.030ms versus 29.020ms, +0.034% time and -0.206% reductions.

Elixir 1.19, 61-sample fixture purchase_order.html: 40.229ms versus 42.049ms, -4.328% time and -0.377% reductions.

Elixir 1.19, 61-sample fixture government_application_form.html: 41.012ms versus 41.216ms, -0.495% time and -0.224% reductions.

Elixir 1.19, 61-sample 100-row HTML render: 156.806ms versus 155.940ms, +0.555% time and +0.045% reductions.

Elixir 1.19, 61-sample 100-row uncompressed full-font render: 138.739ms versus 138.311ms, +0.309% time and +0.036% reductions.

Elixir 1.19, 61-sample repeated text layout: 26.795ms versus 26.593ms, +0.760% time and +0.007% reductions.

Elixir 1.19, 61-sample five repeated PNG images: 13.725ms versus 13.748ms, -0.167% time and -0.002% reductions.

Elixir 1.19, 61-sample 800-word inline paragraph: 43.614ms versus 43.366ms, +0.572% time and +0.040% reductions.

Elixir 1.19, 61-sample 160 table headings: 62.008ms versus 61.703ms, +0.494% time and +0.017% reductions.

Elixir 1.20, 61-sample fixture invoice_012.html: 27.493ms versus 27.518ms, -0.091% time and -0.206% reductions.

Elixir 1.20, 61-sample fixture multi_page_report_012.html: 28.828ms versus 28.957ms, -0.445% time and -0.215% reductions.

Elixir 1.20, 61-sample fixture purchase_order.html: 42.447ms versus 41.277ms, +2.835% time and +0.003% reductions.

Elixir 1.20, 61-sample fixture government_application_form.html: 40.766ms versus 41.049ms, -0.689% time and -0.233% reductions.

Elixir 1.20, 61-sample 100-row HTML render: 145.743ms versus 145.963ms, -0.151% time and +0.006% reductions.

Elixir 1.20, 61-sample 100-row uncompressed full-font render: 132.706ms versus 131.831ms, +0.664% time and +0.008% reductions.

Elixir 1.20, 61-sample repeated text layout: 26.853ms versus 26.846ms, +0.026% time and -0.001% reductions.

Elixir 1.20, 61-sample five repeated PNG images: 13.802ms versus 13.743ms, +0.429% time and -0.004% reductions.

Elixir 1.20, 61-sample 800-word inline paragraph: 41.232ms versus 40.897ms, +0.819% time and +0.020% reductions.

Elixir 1.20, 61-sample 160 table headings: 57.509ms versus 57.730ms, -0.383% time and +0.026% reductions.


First feature samples are exploratory. VM sampling occurs every 10ms and excludes
native or brief peaks. Every generated prefix, visible label and initial editable
field remains in cold and repeated reference checks. Unused template resources
never resolve. Two excluded variants preserve 34 captures each, exact original bytes and complete
source snapshots. An attempted empty-class shortcut increased five-image reductions
by 0.225% on 1.19 and 0.228% on 1.20. Compatible module combinations isolated the
increase to Style using the shortcut; image/font call counts did not increase.
The shortcut was removed. The final parser uses a direct binary escape-unit match
instead of String.starts_with? for every CSS character. The staging pilot preserves
all 21 hashes. Final accepted comparisons in this record determine source acceptance.

Three partial inventory records have updated notes. Categories remain 2,251
implemented, 4,653 partial, 2,239 planned, 421 printable exclusions and 7,703
non-printing records. The audit retains all 17,267 IDs. Remaining CSS, full static
HTML, SVG, MathML and embedded-document dependencies are still milestone work.

## Batch 36: custom containers, initial states and semantic inline markup

Unregistered custom containers use ordinary inline inheritance and supported
transparent flow/phrasing. Canonical names fold ASCII only, preserve Unicode and
valid punctuation, and reject eight reserved names. A trailing slash does not
close a nonvoid custom element. Fragment and supplied-DOM validation share name,
namespace, event and ancestor content rules. Builtin is metadata retains ordinary
control/data-script behavior without upgrades. Custom src/href/control-named
metadata creates no assets, links or fields.

Global hidden defaults to display:none below author CSS. Empty/invalid values,
including false and spaced keywords, mean Hidden. Actual siblings and metadata
processing remain. Unpainted descendants create no resources or fields; an img's
own src retains strict validation even when hidden. Author display overrides use
normal resource policy. Exact ASCII-insensitive until-found diagnoses pending
content-visibility/containment painting. Input type=hidden keeps its separate UA
important rule.

Global inert uses boolean presence and inherited ordinary-tree state. Its initial
text, images, CSS and sanitized control values remain visible, while fields and
link regions are inactive. An inert span inside an active link leaves surrounding
text active. Enabled/disabled selectors and actual DOM order remain. Caller-supplied internal
inert flags are cleared and recomputed from canonical attribute/inherited state. Visible
resources retain normal resolver authorization and diagnostics. Modal/shadow/flat
composition is still open. Browser selection, hit-testing and find actions are
excluded; PDF text remains searchable and extractable.

Mark defaults to yellow background and black text. Ins underlines; del and legacy
strike use line-through. Ins/del contents are transparent through the nearest
ordinary ancestor. Cite/datetime are preserved metadata without fetching or link
activation; edit date parsing and association remain open. Author CSS overrides
colors and default decorations. Empty/nested markup, active ordinary fields,
unused templates, block descendants and public failures are tested.

Inline color backgrounds now paint fragments before their text, retaining ordered
ancestor/child colors and originating font ascent/descent through mixed fonts,
wrapping, relative positioning, atomic contents and page breaks. Rectangle painting
uses the existing four-decimal boundary bias to match Chromium's print-matrix
raster ties. Decorations snap from the painted text baseline before their offset.
The optional internal baseline offset is validated by WriterValidator. Full inline
borders, padding, image backgrounds and interrupted decorated block fragments are
unfinished. Native tags dispatch directly through the canonical builtin map before
custom-name scanning. No new tree scan, policy, limit or diagnostic contract is added.

Twenty-one tests extend six existing unit files. Four parity fixtures retain all
labels, default/override colors, initial values and boxes. Complete native and
pinned scripting-disabled pages were inspected. Custom containers pass at
0.430871%, hidden at 0.301227%, inert at 0.722854%, and semantic inline markup at
0.839360%/0.750402% on two pages. The existing two-page decoration fixture passes
at 0.607587%. Semantic-link artwork explicitly styles underline/blue because full
anchor UA defaults remain open. Ordinary extraction differs only in spacing or
blank lines; editable values are also checked after flattening a verification copy.
Thresholds remain below 1% per page and no content is omitted.

The final matrix passed on supported Elixir 1.19.5/OTP 28 and 1.20.2/OTP 29.
Each runtime passed 1,260 unit tests, 100% coverage, performance, Dialyzer and
119 browser comparisons. Every stage reported zero warnings. Final logs are
`.quality/logs/20261009T041030Z-2642287/`. Exact host parity also passed all 119 cases without warnings. Final source
comparisons and the longer trials below are verified before acceptance. Earlier custom/hidden root evidence passed 1,250 tests/100%/117 comparisons but is intermediate evidence.
Its 52 reports, original bytes and 78 source files are archived under ignored
.quality/performance/excluded-captures/1.1.0-batch-36-before-static-composition and
.quality/performance/snapshots/batch-36-before-static-composition.

The final seven-module shared-context profile keeps Font, FontFallback, PNG,
Layout and writer call counts unchanged in ordinary tables. Per render it adds
5,472 custom-name dispatches, 1,216 inert-state delegations, 2,330 hidden-state
lookups and 2,432 canonical HTML-element checks. DOM recursion retains its call
count with an ancestor argument, and name tokenization retains its call count
with scanner state. ASCII folding replaces the existing Unicode folding calls.
Inertness travels in the existing selector traversal. No image or font is read
again and there is no extra tree traversal. Instrumented times are excluded.
Final profile log is `.quality/batch-36-followup-table-shared-1.20.log`. A faster native dispatch pilot removes binary name testing on builtin
tags; its 100-row 1.20 pilot removes 16,166 reductions, or 0.140366%. Source bytes
are preserved and pilot timing is excluded. Intermediate 1.19 full-font timing
increased 4.146% in 31 samples and 5.783% in 61, requiring further investigation.
Four compatible module factors retain complete PDF equality; their diagnostic
medians span 182.983-197.054ms, with parser-only +0.147%, policy-only -4.667%, and
combined +2.664% relative to 191.941ms. These instrument/source probes do not accept
a regression or replace final measurements. Controlled final source comparisons
retain fee6c4d and accepted B35, with default guardrails unchanged.

The final 61-sample trials reduce full-font timing to +0.333% on 1.19 and
-0.947% on 1.20, versus +1.401%/+2.199% in the first final trials. The earlier
intermediate +4.146%/+5.783% result is not reproduced by the accepted sources.
Full-font reductions change by +32,366, +0.268%, and -3,021, -0.028%. The longer
compressed-table trials change by -0.110%/+0.016% time and +32,308/+34,910
reductions, +0.253%/+0.304%. Headings change by -2.437%/+0.887% time and
+18,743/+22,435 reductions, +0.387%/+0.475%. Their first timing changes were
-0.371%/-8.985%, so no stable headings slowdown is established.

The earlier 1.20 repeated-text +6.636%, ten-image +5.127% and visual-extraction
+4.465% gaps shrink to +0.405%, +0.024% and +0.003% in the longer trials. The
1.19 field-flatten +3.278% gap becomes +0.068%; 1.20 flatten is +0.045%.
Complete outputs remain identical throughout. Invoice reductions add
3,298/3,859, +0.152%/+0.185%. Longer invoice timing is +1.995%/+1.025%, while
fresh trials are -0.305%/-0.639% and fixed paired trials are -1.433%/-2.445%.
Those trials do not establish a repeatable invoice slowdown beyond variation.

The small remaining reduction costs implement canonical supplied-DOM name and
ancestor validation, initial hidden/inert state and validated rectangle baseline
metadata. These guarantees apply to every HTML tree, including ordinary trees.
The profile does not assign an exact share of each reduction delta to a function;
GC and heap state also vary. Native-map dispatch removes avoidable builtin name
scanning, ASCII folding replaces Unicode folding, and inertness shares the
existing selector traversal. No duplicate resource work or extra scan remains.
Removing the canonical checks would allow invalid supplied names/content or
caller-supplied state to bypass the documented semantics. Further validation reuse
would require a private pipeline design that preserves each public validator's
guarantees. This measured correctness cost is recorded, not charged against the
5% guardrail allowance.

The fixed baseline is retained. Its warmed 1.19 compressed-table timing is
+10.839% in the paired aggregate, while isolated candidate/previous timing is
149.461ms/147.885ms and longer timing is 154.996ms/155.166ms. The fixed warmed
comparison and isolated trials use different process histories. Cumulative
milestone performance remains unresolved and must be audited before release.
This batch does not declare the complete 1.1.0 performance requirement satisfied.

Full HTML, SVG, MathML and embedded documents remain unfinished. Until-found,
popover, flat-tree/dialog inertness, edit date semantics and remaining inline box
composition are still planned. Directional layout retains its 1.2.0 dependency.


88 final captures are consolidated in ignored
`.quality/performance/results/1.1.0-batch-36/reports.json`. Twelve retain the fixed
fee6c4d baseline, previous B35 and candidate in both revision orders on CPUs 4,5.
Custom, hidden, inert and semantic-inline workloads each retain twelve 1/10/100-instance static/interactive
captures and six nesting captures at depths 1/10/50. Two alternate 31 fresh-process
samples for all 19 original operations and ordinary Text-field fill/flatten.
All 78 candidate source hashes match the accepted files. Guardrails and fixed
baseline remain unchanged; cumulative milestone performance remains open.

| Runtime and ordinary workload | Candidate ms | Previous ms | Time change | Reductions change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 28.067 | 28.153 | -0.305% | -0.006% |
| 1.19 fixture statement_012.html | 27.840 | 28.271 | -1.525% | +0.061% |
| 1.19 fixture multi_page_report_012.html | 30.209 | 30.157 | +0.172% | +0.025% |
| 1.19 fixture purchase_order.html | 44.411 | 45.042 | -1.401% | +0.043% |
| 1.19 fixture material_requisition.html | 63.557 | 64.194 | -0.992% | +0.119% |
| 1.19 fixture government_application_form.html | 43.078 | 42.976 | +0.237% | +0.044% |
| 1.19 fixture stock_sticker.html | 12.121 | 11.950 | +1.431% | -0.157% |
| 1.19 fixture trim_card.html | 53.831 | 52.428 | +2.676% | +0.246% |
| 1.19 100-row HTML render | 149.461 | 147.885 | +1.066% | +0.132% |
| 1.19 100-row uncompressed full-font render | 140.166 | 138.229 | +1.401% | +0.215% |
| 1.19 100-row PDF text extraction | 70.635 | 71.387 | -1.053% | -0.008% |
| 1.19 100-row PDF visual text extraction | 73.930 | 74.867 | -1.252% | -0.011% |
| 1.19 repeated text layout | 27.237 | 27.312 | -0.275% | +0.064% |
| 1.19 long paragraph layout | 50.785 | 51.886 | -2.122% | +0.010% |
| 1.19 five repeated PNG images | 14.096 | 13.996 | +0.714% | +0.008% |
| 1.19 ten repeated 512px PNG images | 341.529 | 343.722 | -0.638% | -0.026% |
| 1.19 800-word inline paragraph | 46.333 | 45.871 | +1.007% | +0.196% |
| 1.19 160 table headings | 64.981 | 65.223 | -0.371% | +0.693% |
| 1.19 4,000 boxes on one page | 15.722 | 15.451 | +1.754% | +0.003% |
| 1.19 100 ordinary Text-field fill | 470.201 | 467.775 | +0.519% | +0.023% |
| 1.19 100 ordinary Text-field flatten | 79.645 | 77.117 | +3.278% | +0.003% |
| 1.20 fixture invoice_012.html | 29.532 | 29.722 | -0.639% | +0.156% |
| 1.20 fixture statement_012.html | 27.484 | 27.158 | +1.200% | +0.102% |
| 1.20 fixture multi_page_report_012.html | 31.265 | 30.356 | +2.994% | +0.193% |
| 1.20 fixture purchase_order.html | 44.259 | 44.209 | +0.113% | +0.111% |
| 1.20 fixture material_requisition.html | 66.343 | 64.418 | +2.988% | +0.007% |
| 1.20 fixture government_application_form.html | 44.199 | 44.367 | -0.379% | +0.045% |
| 1.20 fixture stock_sticker.html | 12.863 | 13.029 | -1.274% | +0.107% |
| 1.20 fixture trim_card.html | 54.907 | 54.774 | +0.243% | +0.132% |
| 1.20 100-row HTML render | 156.897 | 158.599 | -1.073% | +0.793% |
| 1.20 100-row uncompressed full-font render | 143.080 | 140.002 | +2.199% | +0.627% |
| 1.20 100-row PDF text extraction | 70.894 | 71.181 | -0.403% | -0.028% |
| 1.20 100-row PDF visual text extraction | 119.301 | 114.202 | +4.465% | +0.052% |
| 1.20 repeated text layout | 41.250 | 38.683 | +6.636% | +0.096% |
| 1.20 long paragraph layout | 45.546 | 45.587 | -0.090% | -0.012% |
| 1.20 five repeated PNG images | 13.859 | 13.742 | +0.851% | +0.010% |
| 1.20 ten repeated 512px PNG images | 423.188 | 402.550 | +5.127% | +0.244% |
| 1.20 800-word inline paragraph | 45.182 | 45.079 | +0.228% | +0.024% |
| 1.20 160 table headings | 68.983 | 75.793 | -8.985% | +0.640% |
| 1.20 4,000 boxes on one page | 13.002 | 13.071 | -0.528% | +0.021% |
| 1.20 100 ordinary Text-field fill | 567.462 | 577.413 | -1.723% | -0.060% |
| 1.20 100 ordinary Text-field flatten | 72.369 | 74.178 | -2.439% | -0.056% |

All 21 complete ordinary outputs retain identical hashes.

The longer 61-sample trials retain the earlier results and repeat fourteen operations.

| Runtime and repeated workload | Candidate ms | Previous ms | Time change | Reductions change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 28.522 | 27.964 | +1.995% | +0.152% |
| 1.19 fixture multi_page_report_012.html | 30.306 | 30.587 | -0.919% | +0.049% |
| 1.19 fixture purchase_order.html | 49.874 | 52.480 | -4.966% | +0.005% |
| 1.19 fixture government_application_form.html | 59.437 | 59.891 | -0.758% | +0.029% |
| 1.19 100-row HTML render | 154.996 | 155.167 | -0.110% | +0.253% |
| 1.19 100-row uncompressed full-font render | 137.534 | 137.077 | +0.333% | +0.268% |
| 1.19 100-row PDF visual text extraction | 68.713 | 68.830 | -0.170% | -0.000% |
| 1.19 repeated text layout | 26.405 | 26.489 | -0.317% | +0.063% |
| 1.19 five repeated PNG images | 13.755 | 13.757 | -0.015% | +0.008% |
| 1.19 ten repeated 512px PNG images | 332.847 | 333.496 | -0.195% | -0.019% |
| 1.19 800-word inline paragraph | 40.831 | 41.322 | -1.188% | -0.019% |
| 1.19 160 table headings | 55.327 | 56.709 | -2.437% | +0.387% |
| 1.19 100 ordinary Text-field fill | 396.701 | 397.056 | -0.089% | -0.001% |
| 1.19 100 ordinary Text-field flatten | 72.481 | 72.432 | +0.068% | +0.005% |
| 1.20 fixture invoice_012.html | 26.607 | 26.337 | +1.025% | +0.185% |
| 1.20 fixture multi_page_report_012.html | 28.026 | 28.239 | -0.754% | +0.040% |
| 1.20 fixture purchase_order.html | 42.236 | 41.977 | +0.617% | +0.134% |
| 1.20 fixture government_application_form.html | 39.960 | 39.774 | +0.468% | +0.061% |
| 1.20 100-row HTML render | 141.768 | 141.745 | +0.016% | +0.304% |
| 1.20 100-row uncompressed full-font render | 129.349 | 130.586 | -0.947% | -0.028% |
| 1.20 100-row PDF visual text extraction | 67.018 | 67.016 | +0.003% | -0.014% |
| 1.20 repeated text layout | 26.278 | 26.172 | +0.405% | +0.075% |
| 1.20 five repeated PNG images | 13.547 | 13.544 | +0.022% | +0.010% |
| 1.20 ten repeated 512px PNG images | 330.622 | 330.543 | +0.024% | +0.000% |
| 1.20 800-word inline paragraph | 40.471 | 40.426 | +0.111% | +0.010% |
| 1.20 160 table headings | 56.216 | 55.722 | +0.887% | +0.475% |
| 1.20 100 ordinary Text-field fill | 390.121 | 391.089 | -0.248% | +0.002% |
| 1.20 100 ordinary Text-field flatten | 68.332 | 68.301 | +0.045% | +0.008% |

custom-containers, Elixir 1.19, 100 static instances: 81.158ms versus 64.991ms equivalent references; +556,208 reductions. First render 158.999ms, sampled VM peak 84,351,216 bytes and output 90,283 bytes.

custom-containers, Elixir 1.19, 100 interactive instances: 114.940ms versus 102.052ms equivalent references; +531,731 reductions. First render 195.191ms, sampled VM peak 87,280,896 bytes and output 336,613 bytes.

custom-containers, Elixir 1.20, 100 static instances: 84.835ms versus 64.153ms equivalent references; +486,306 reductions. First render 152.631ms, sampled VM peak 87,087,176 bytes and output 90,283 bytes.

custom-containers, Elixir 1.20, 100 interactive instances: 112.598ms versus 98.065ms equivalent references; +457,179 reductions. First render 190.231ms, sampled VM peak 84,829,400 bytes and output 336,613 bytes.

hidden-attribute, Elixir 1.19, 100 static instances: 119.951ms versus 105.654ms equivalent references; -185,409 reductions. First render 243.755ms, sampled VM peak 77,157,432 bytes and output 88,101 bytes.

hidden-attribute, Elixir 1.19, 100 interactive instances: 112.479ms versus 111.878ms equivalent references; -201,183 reductions. First render 204.800ms, sampled VM peak 86,995,968 bytes and output 334,733 bytes.

hidden-attribute, Elixir 1.20, 100 static instances: 66.651ms versus 67.497ms equivalent references; -242,337 reductions. First render 153.005ms, sampled VM peak 76,429,368 bytes and output 88,101 bytes.

hidden-attribute, Elixir 1.20, 100 interactive instances: 101.268ms versus 102.452ms equivalent references; -254,866 reductions. First render 184.403ms, sampled VM peak 78,807,360 bytes and output 334,733 bytes.

inert-attribute, Elixir 1.19, 100 static instances: 70.440ms versus 67.423ms equivalent references; +47,454 reductions. First render 151.868ms, sampled VM peak 76,090,096 bytes and output 93,422 bytes.

inert-attribute, Elixir 1.19, 100 interactive instances: 72.373ms versus 69.682ms equivalent references; +56,754 reductions. First render 153.456ms, sampled VM peak 71,588,488 bytes and output 93,422 bytes.

inert-attribute, Elixir 1.20, 100 static instances: 67.378ms versus 65.817ms equivalent references; +91,110 reductions. First render 145.584ms, sampled VM peak 77,906,248 bytes and output 93,422 bytes.

inert-attribute, Elixir 1.20, 100 interactive instances: 73.850ms versus 79.494ms equivalent references; +72,715 reductions. First render 143.973ms, sampled VM peak 75,001,032 bytes and output 93,422 bytes.

semantic-inline, Elixir 1.19, 100 static instances: 117.322ms versus 127.712ms equivalent references; -84,855 reductions. First render 206.756ms, sampled VM peak 84,778,872 bytes and output 178,243 bytes.

semantic-inline, Elixir 1.19, 100 interactive instances: 182.918ms versus 181.592ms equivalent references; -5,348 reductions. First render 254.719ms, sampled VM peak 87,395,728 bytes and output 424,989 bytes.

semantic-inline, Elixir 1.20, 100 static instances: 132.731ms versus 138.362ms equivalent references; -42,524 reductions. First render 192.432ms, sampled VM peak 81,571,032 bytes and output 178,243 bytes.

semantic-inline, Elixir 1.20, 100 interactive instances: 169.227ms versus 160.230ms equivalent references; -47,236 reductions. First render 237.494ms, sampled VM peak 88,363,480 bytes and output 424,989 bytes.

First feature samples are exploratory. VM samples occur every 10ms and omit native
or brief peaks. Every cold/repeated complete reference PDF, label and initial
editable name/value is verified. Dormant dependencies never resolve. Custom-only
and hidden-only stage pilots retain exact source bytes but are excluded from
accepted-source performance. Their separate staging directories are diagnostic
history; both features ship together in B36.

Thirty custom, initial-state and semantic-inline records move to partial, with related notes updated. Counts are
2,251 implemented, 4,683 partial, 2,209 planned, 421 printable exclusions and
7,703 non-printing records. All 17,267 IDs remain. Exact until-found painting,
initial popover behavior and complete inert composition, complete HTML, SVG, MathML and child documents
remain unfinished. Directional layout stays scheduled for 1.2.0.

## Batch 37: initial anchor appearance

Fresh unvisited anchors with supported href destinations default to #0000EE and
underline below author CSS. Anchors without href retain inherited color and
ordinary decorations. Nested text and wrapped fragments retain their origin's
underline and active URI regions. Inert anchors keep this artwork while omitting
active regions; hidden anchors remain unpainted. Author color/decorations retain
precedence. HTTP, HTTPS and mailto validation and resource authorization remain
unchanged, and links never fetch their destinations.

Only Style changes. Two tests extend the existing Style and HtmlToPdf test files;
the existing Layout test additionally checks underline geometry, color and URI
bounds. The earlier inert reference now explicitly preserves the inactive link's
color and underline after removing href. Both static/interactive render modes
retain whole reference PDFs and initial values. Public invalid URI inputs retain
the shared diagnostic shape. No helper, configurable limit or tree traversal is
added.

The staging run passes 1,262 tests and 100% coverage. Three complete native and
Chromium page pairs were inspected. The new initial-default fixture passes at
0.989899%, the existing inert fixture at 0.722854%, and existing protocol/entity
fixture at 0.091190%. All extracted initial-default labels and spacing match.
The unchanged per-page threshold remains below 1%. No content is removed.
The final root matrix passes supported Elixir 1.19.5/OTP 28 and 1.20.2/OTP 29.
Each runtime passes 1,262 unit tests, 100% coverage, unchanged performance
guardrails, Dialyzer and 120 browser comparisons. Every stage reports zero
warnings. Logs are `.quality/logs/20261009T045416Z-2751330/`. Exact host parity
also passes all 120 cases without warnings. Final source comparisons preserve every complete ordinary and feature-reference PDF. No repeatable new timing cost appears in fresh-process comparisons.

The excluded 15-sample ordinary pilot preserves all 21 complete output hashes and
all 78 source files under `.quality/performance/diagnostic-captures/batch-37-anchor-stage-pilot/`.
Its table reduction change is +0.007645%, full-font +0.012721%, and five-image
+0.002848%; pilot timing is excluded from final acceptance. A separate feature
pilot retains complete authored reference PDFs, active URI annotations, inactive
inert links, every label and initial field value. First-render and sampled VM
values are exploratory and exclude native or brief peaks.

Anchor construction, transparent block/image contents, remaining attributes,
base/relative/fragment destinations and clipped regions remain open. Full HTML,
SVG, MathML and controlled child documents remain unfinished. Directional layout
retains its 1.2.0 dependency. Broad anchor/default records remain partial.


The fixed harness retains both revision orders and sampled VM memory. Its warmed
single-process history gives ten-image 1.20 timing changes of +43.52% and +47.91%
and inline-paragraph 1.19 changes of +12.60% and +7.04%. These observations remain
in the record. The 31-sample fresh-process comparison removes inherited heaps
from earlier operations: ten-image +0.120% and inline-paragraph -1.377%, with
+0.002% and -0.017% reductions respectively. A further 61-sample comparison
alternates only Style in the same VM, with identical inputs and complete output
hashes. It investigates tables, full-font tables, ten images and inline paragraphs
on both supported runtimes. Load, warmup and collection stay outside timing;
per-sample GC statistics and all 78 source hashes remain in the captures. Only
Style differs from accepted B36, and ordinary image/font modules remain identical.
The warmed differences do not reproduce under controlled process histories.
This does not close the cumulative fixed-baseline release-performance audit.

| Runtime and investigated workload | Candidate ms | Previous ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 100-row HTML render | 146.092 | 146.622 | -0.361% | +0.001% |
| 1.19 100-row uncompressed full-font render | 137.421 | 137.202 | +0.160% | -0.002% |
| 1.19 ten repeated 512px PNG images | 340.837 | 341.715 | -0.257% | -0.010% |
| 1.19 800-word inline paragraph | 42.404 | 42.208 | +0.464% | +0.016% |
| 1.20 100-row HTML render | 142.526 | 142.709 | -0.128% | +0.001% |
| 1.20 100-row uncompressed full-font render | 132.500 | 132.815 | -0.237% | -0.016% |
| 1.20 ten repeated 512px PNG images | 338.776 | 337.832 | +0.279% | -0.010% |
| 1.20 800-word inline paragraph | 40.634 | 40.707 | -0.179% | -0.007% |


34 accepted-source captures are consolidated at `.quality/performance/results/1.1.0-batch-37/reports.json`. Twelve retain fee6c4d, previous B36 and candidate in both revision orders on CPUs 4,5. Twelve feature captures retain 1/10/100 instances in static and interactive modes; six nesting captures retain depths 1/10/50. Two comparisons alternate 31 fresh-process samples on all 21 ordinary operations. Every candidate matches all 78 source hashes; complete ordinary/reference PDFs and frozen inputs are verified. Guardrails and the fixed baseline remain unchanged.

| Runtime and ordinary workload | Candidate ms | Previous ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 27.184 | 27.287 | -0.377% | -0.009% |
| 1.19 fixture statement_012.html | 26.841 | 26.880 | -0.145% | +0.011% |
| 1.19 fixture multi_page_report_012.html | 28.999 | 29.024 | -0.086% | +0.000% |
| 1.19 fixture purchase_order.html | 40.116 | 40.083 | +0.082% | -0.020% |
| 1.19 fixture material_requisition.html | 57.962 | 58.078 | -0.200% | +0.014% |
| 1.19 fixture government_application_form.html | 40.853 | 40.646 | +0.509% | +0.024% |
| 1.19 fixture stock_sticker.html | 12.044 | 12.158 | -0.938% | +0.012% |
| 1.19 fixture trim_card.html | 50.739 | 50.659 | +0.158% | -0.021% |
| 1.19 100-row HTML render | 148.483 | 148.493 | -0.007% | +0.003% |
| 1.19 100-row uncompressed full-font render | 138.838 | 138.524 | +0.227% | +0.006% |
| 1.19 100-row PDF text extraction | 67.726 | 67.870 | -0.212% | -0.003% |
| 1.19 100-row PDF visual text extraction | 70.412 | 69.768 | +0.923% | +0.014% |
| 1.19 repeated text layout | 25.967 | 26.260 | -1.116% | -0.001% |
| 1.19 long paragraph layout | 64.326 | 64.524 | -0.307% | +0.001% |
| 1.19 five repeated PNG images | 13.875 | 13.826 | +0.354% | +0.002% |
| 1.19 ten repeated 512px PNG images | 331.031 | 332.174 | -0.344% | +0.004% |
| 1.19 800-word inline paragraph | 42.845 | 43.443 | -1.377% | -0.017% |
| 1.19 160 table headings | 56.393 | 56.373 | +0.035% | +0.010% |
| 1.19 4,000 boxes on one page | 14.517 | 14.489 | +0.193% | -0.010% |
| 1.19 100 ordinary Text-field fill | 398.708 | 400.074 | -0.341% | -0.004% |
| 1.19 100 ordinary Text-field flatten | 74.066 | 73.922 | +0.195% | +0.015% |
| 1.20 fixture invoice_012.html | 26.862 | 26.733 | +0.483% | -0.012% |
| 1.20 fixture statement_012.html | 26.221 | 26.223 | -0.008% | +0.013% |
| 1.20 fixture multi_page_report_012.html | 28.451 | 28.520 | -0.242% | +0.004% |
| 1.20 fixture purchase_order.html | 41.281 | 41.345 | -0.155% | +0.032% |
| 1.20 fixture material_requisition.html | 55.443 | 55.324 | +0.215% | +0.016% |
| 1.20 fixture government_application_form.html | 39.892 | 39.744 | +0.372% | +0.005% |
| 1.20 fixture stock_sticker.html | 11.410 | 11.491 | -0.705% | -0.019% |
| 1.20 fixture trim_card.html | 49.832 | 49.190 | +1.305% | +0.021% |
| 1.20 100-row HTML render | 143.324 | 143.749 | -0.296% | +0.005% |
| 1.20 100-row uncompressed full-font render | 129.539 | 129.077 | +0.358% | +0.010% |
| 1.20 100-row PDF text extraction | 64.969 | 65.298 | -0.504% | -0.001% |
| 1.20 100-row PDF visual text extraction | 66.708 | 66.671 | +0.055% | -0.003% |
| 1.20 repeated text layout | 26.359 | 26.342 | +0.065% | +0.005% |
| 1.20 long paragraph layout | 44.539 | 44.930 | -0.870% | +0.005% |
| 1.20 five repeated PNG images | 13.573 | 13.566 | +0.052% | +0.000% |
| 1.20 ten repeated 512px PNG images | 328.318 | 327.924 | +0.120% | +0.002% |
| 1.20 800-word inline paragraph | 41.057 | 41.351 | -0.711% | -0.015% |
| 1.20 160 table headings | 58.457 | 58.620 | -0.278% | -0.000% |
| 1.20 4,000 boxes on one page | 11.884 | 11.969 | -0.710% | -0.005% |
| 1.20 100 ordinary Text-field fill | 397.081 | 395.790 | +0.326% | +0.005% |
| 1.20 100 ordinary Text-field flatten | 67.425 | 66.981 | +0.663% | +0.014% |

Elixir 1.19, 100 static instances: 98.608ms versus authored 105.818ms, -391,621 reductions. First render 179.596ms; sampled VM peak 83,073,744 bytes; output 134,370 bytes.

Elixir 1.19, 100 interactive instances: 153.962ms versus authored 159.388ms, -383,765 reductions. First render 243.028ms; sampled VM peak 77,982,264 bytes; output 386,069 bytes.

Elixir 1.20, 100 static instances: 93.676ms versus authored 95.092ms, -252,045 reductions. First render 163.831ms; sampled VM peak 84,064,632 bytes; output 134,370 bytes.

Elixir 1.20, 100 interactive instances: 146.243ms versus authored 149.598ms, -323,507 reductions. First render 227.250ms; sampled VM peak 79,275,712 bytes; output 386,069 bytes.

Three broad anchor/UA records retain partial status. Counts remain 2,251 implemented, 4,683 partial, 2,209 planned, 421 printable exclusions and 7,703 non-printing records. The audit preserves all 17,267 IDs. Cumulative milestone performance remains open.

## Batch 38: inline word boundaries and wrapping properties

Ordinary inline markup no longer introduces a wrapping opportunity. A word split
by bold, links, highlights or relative inline containers moves as a whole when
an earlier ordinary opportunity exists. An indivisible word may overflow.
Whitespace, supported hyphens, hard breaks and atomic boxes retain their distinct
behavior. Emergency wrapping preserves extended grapheme clusters across styled
boundaries and retains text, decorations, backgrounds and interactive metadata.

CSS `line-break`, `overflow-wrap` and `word-break` now retain separate inherited
values. `line-break:anywhere` permits ordinary grapheme opportunities.
`overflow-wrap:anywhere` and `break-word` use emergency opportunities after ordinary
ones. Their min-content behavior differs: `anywhere` contributes grapheme widths,
while `break-word` retains word widths. The supported `word-break:break-word`
legacy shorthand uses emergency wrapping with grapheme min-content widths.
Invalid CSS keyword combinations and malformed advanced styled trees use the
shared diagnostics contract through HtmlValidator. The legacy advanced
`:line_break => :break_word` value remains supported.

The CSS rules were checked against CSS Text Level 3, dated 2026-08-14,
https://www.w3.org/TR/2026/CRD-css-text-3-20260814/ . The HTML inventory retains
its pinned 2026-10-05 snapshot, SHA-256
`31f43282e555f90dce3c860356cae3a3a992dbf434dadf21060ef960825d07a8`.
The wrapping correction is a prerequisite for optional HTML word breaks.
`wbr` and `nobr` remain staged for the next batch.

Eight focused tests extend the existing Style, Layout, HtmlValidator and public
HTML renderer files. Complete authored reference PDFs preserve URI annotations
and initial Text fields in static and interactive modes. The two-page
`html_inline_word_boundaries.html` fixture retains all 13 cases, including
combining marks split across styles, whitespace modes and a narrow overflowing
word. Its pages pass at 0.818182% and 0.584596% changed pixels. Native/Chromium
extracted text matches exactly and complete pages were inspected. Thresholds,
fonts and difficult cases remain unchanged.

The previous wrapping representation conflated wrapping properties and performed unnecessary
map lookups. Tuple/mapping dispatch and a fast validator path remove those
lookups. A subsequent profile found an avoidable reverse/find scan for each
ordinary overflowing token. Tokens from one run already encode their ordinary
break opportunities, so that path now recognizes their shared merge key before
scanning styled boundaries. The fallback preserves empty relative markers and
cross-style boundaries. Instrumented profile timings are diagnostic only.
Earlier source and capture bytes remain under ignored diagnostic directories;
final-source acceptance captures are separate.

Full Unicode line-breaking rules, language-specific behavior, shaping and grouped
styled-word min-content widths remain open. The broad white-space record remains
partial. Cumulative milestone performance, including the existing fixed-baseline
table regression, remains open; this batch does not reset fee6c4d or declare the
1.1.0 plan complete.


34 accepted-source captures are consolidated at `.quality/performance/results/1.1.0-batch-38/reports.json`. Twelve retain fee6c4d, previous B37 and candidate in both revision orders on CPUs 4,5. Twelve feature captures retain 1/10/100 instances in static and interactive modes; six nesting captures retain depths 1/10/50. Two comparisons alternate 31 fresh-process samples on all 21 ordinary operations. Two follow-ups retain 61 samples per revision on four targeted operations. Every candidate matches all 78 source hashes; complete ordinary/reference PDFs and frozen inputs are verified. Guardrails and the fixed baseline remain unchanged.

| Runtime and ordinary workload | Candidate ms | Previous ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 29.013 | 29.226 | -0.729% | +0.054% |
| 1.19 fixture statement_012.html | 28.561 | 28.666 | -0.366% | +0.024% |
| 1.19 fixture multi_page_report_012.html | 31.137 | 31.145 | -0.026% | +0.053% |
| 1.19 fixture purchase_order.html | 44.180 | 43.572 | +1.395% | +0.027% |
| 1.19 fixture material_requisition.html | 65.828 | 65.352 | +0.728% | +0.072% |
| 1.19 fixture government_application_form.html | 43.020 | 43.172 | -0.352% | +0.006% |
| 1.19 fixture stock_sticker.html | 12.575 | 12.693 | -0.930% | +0.036% |
| 1.19 fixture trim_card.html | 55.478 | 54.032 | +2.676% | +0.109% |
| 1.19 100-row HTML render | 162.853 | 162.118 | +0.453% | +0.025% |
| 1.19 100-row uncompressed full-font render | 144.724 | 147.387 | -1.807% | -0.133% |
| 1.19 100-row PDF text extraction | 73.205 | 72.402 | +1.109% | +0.002% |
| 1.19 100-row PDF visual text extraction | 73.846 | 73.273 | +0.782% | -0.001% |
| 1.19 repeated text layout | 26.985 | 26.752 | +0.871% | +0.026% |
| 1.19 long paragraph layout | 68.930 | 68.764 | +0.241% | +0.004% |
| 1.19 five repeated PNG images | 13.875 | 13.833 | +0.304% | +0.000% |
| 1.19 ten repeated 512px PNG images | 347.588 | 350.320 | -0.780% | -0.029% |
| 1.19 800-word inline paragraph | 47.017 | 46.691 | +0.698% | +0.050% |
| 1.19 160 table headings | 59.938 | 59.716 | +0.372% | +0.155% |
| 1.19 4,000 boxes on one page | 15.346 | 15.403 | -0.370% | -0.002% |
| 1.19 100 ordinary Text-field fill | 563.516 | 558.125 | +0.966% | +0.007% |
| 1.19 100 ordinary Text-field flatten | 72.265 | 73.037 | -1.057% | -0.002% |
| 1.20 fixture invoice_012.html | 29.151 | 28.982 | +0.583% | +0.047% |
| 1.20 fixture statement_012.html | 28.188 | 27.963 | +0.805% | +0.011% |
| 1.20 fixture multi_page_report_012.html | 30.713 | 30.862 | -0.483% | +0.042% |
| 1.20 fixture purchase_order.html | 43.085 | 42.792 | +0.685% | +0.020% |
| 1.20 fixture material_requisition.html | 62.268 | 62.183 | +0.137% | +0.051% |
| 1.20 fixture government_application_form.html | 42.524 | 42.654 | -0.305% | -0.004% |
| 1.20 fixture stock_sticker.html | 12.431 | 12.576 | -1.153% | +0.022% |
| 1.20 fixture trim_card.html | 54.535 | 54.213 | +0.594% | +0.111% |
| 1.20 100-row HTML render | 153.133 | 150.275 | +1.902% | +0.103% |
| 1.20 100-row uncompressed full-font render | 137.704 | 137.088 | +0.449% | +0.056% |
| 1.20 100-row PDF text extraction | 71.992 | 71.927 | +0.090% | +0.002% |
| 1.20 100-row PDF visual text extraction | 74.756 | 74.649 | +0.143% | -0.007% |
| 1.20 repeated text layout | 27.360 | 27.258 | +0.374% | +0.027% |
| 1.20 long paragraph layout | 46.329 | 46.332 | -0.006% | +0.009% |
| 1.20 five repeated PNG images | 13.813 | 13.801 | +0.087% | -0.002% |
| 1.20 ten repeated 512px PNG images | 339.430 | 339.642 | -0.062% | +0.011% |
| 1.20 800-word inline paragraph | 45.250 | 45.078 | +0.382% | +0.017% |
| 1.20 160 table headings | 58.658 | 60.694 | -3.355% | -0.077% |
| 1.20 4,000 boxes on one page | 13.095 | 13.111 | -0.122% | -0.002% |
| 1.20 100 ordinary Text-field fill | 574.964 | 570.190 | +0.837% | +0.034% |
| 1.20 100 ordinary Text-field flatten | 71.792 | 70.459 | +1.892% | +0.019% |

Elixir 1.19, 100 static instances: 164.917ms versus authored 156.648ms, +199,484 reductions. First render 238.662ms; sampled VM peak 95,538,328 bytes; output 226,847 bytes.

Elixir 1.19, 100 interactive instances: 211.587ms versus authored 222.546ms, +121,354 reductions. First render 322.248ms; sampled VM peak 84,867,096 bytes; output 477,431 bytes.

Elixir 1.20, 100 static instances: 136.607ms versus authored 139.491ms, +211,638 reductions. First render 215.657ms; sampled VM peak 84,500,680 bytes; output 226,847 bytes.

Elixir 1.20, 100 interactive instances: 205.395ms versus authored 225.862ms, +198,910 reductions. First render 293.873ms; sampled VM peak 84,156,928 bytes; output 477,431 bytes.

One broad whitespace record retain partial status. Counts remain 2,251 implemented, 4,683 partial, 2,209 planned, 421 printable exclusions and 7,703 non-printing records. The audit preserves all 17,267 IDs. Cumulative milestone performance remains open.

Final matrix `.quality/logs/20261009T061446Z-2958869/` passes Elixir 1.19.5/OTP 28 and Elixir 1.20.2/OTP 29 with 1,270 unit tests, 100% coverage, all 121 parity comparisons and zero warnings. The exact host command passes 121 comparisons on Elixir 1.20.4/OTP 29.

The 61-sample follow-up preserves output equality for four targeted workloads on both runtimes.

| Runtime and workload | Candidate ms | Previous ms | Time change | Extra reductions | Reduction change |
| --- | --- | --- | --- | --- | --- |
| 1.19 100-row HTML render | 159.482 | 159.870 | -0.243% | +5,646 | +0.044% |
| 1.19 100-row uncompressed full-font render | 151.027 | 150.681 | +0.230% | +5,417 | +0.045% |
| 1.19 repeated text layout | 28.578 | 28.450 | +0.450% | +702 | +0.023% |
| 1.19 800-word inline paragraph | 46.598 | 46.958 | -0.767% | +38 | +0.001% |
| 1.20 100-row HTML render | 151.942 | 151.896 | +0.030% | +8,273 | +0.072% |
| 1.20 100-row uncompressed full-font render | 136.797 | 136.829 | -0.023% | +5,727 | +0.053% |
| 1.20 repeated text layout | 27.237 | 27.205 | +0.118% | +777 | +0.025% |
| 1.20 800-word inline paragraph | 44.695 | 44.551 | +0.323% | +1,303 | +0.033% |

After removing the overflow scan, the table costs 5,646/8,273 additional reductions, +0.044%/+0.072%, with elapsed changes -0.243%/+0.030% on 1.19/1.20. Full-font tables add 5,417/5,727 reductions, +0.045%/+0.053%, with +0.230%/-0.023% elapsed changes. The inline paragraph adds 38/1,303 reductions, +0.001%/+0.033%, with -0.767%/+0.323% elapsed changes. The earlier approximately 0.24% inline reduction increase was removed. Remaining small work comes from canonical per-node wrapping validation and retaining independent wrapping properties through text styles. Removing validation would leave advanced styled input unchecked; conflating properties would reintroduce incorrect wrapping and min-content behavior. A separate all-normal pattern pilot did not reduce ordinary table work because ordinary styles already take the absent-property path; its source/results remain excluded diagnostics. Future work can reduce style copying while retaining these guarantees. Instrumented eprof logs establish call paths, not accepted elapsed times.

## Batch 39: optional breaks and legacy nowrap

Void `<wbr>` elements create optional opportunities without a substitute glyph.
Legacy `<nobr>` containers apply nowrap below author CSS; descendant wbr receives
the specified normal-whitespace default. Explicit opportunities retain whole
styled words, parent line metrics, relative contexts, pagination and URI regions.
Empty and consecutive markers retain a line. Forced breaks and preserved blank
lines also retain nonpainting pagination geometry. The 100-instance complete-PDF
check caught an existing br bug that discarded a blank line at a page boundary;
Chromium confirmed the retained line. Focused geometry, complete-PDF and browser
checks cover both break types at automatic page boundaries. A leading marker can produce a blank
line before an oversized indivisible word. Authored display:none hides a marker.
Other own box styles, background resources, pseudo content and counter effects
produce no paint or asset reads. Empty flex/grid items retain their slots or gaps.

Canonical validators own tag, void-child, parent-context and styled-tree policy.
Malformed DOM/style/flow-marker input returns the shared diagnostic tuple. Writer
normalization counts markers toward cumulative layout limits before removing them;
markers cannot carry text, paint, images, glyphs, links or field controls.

Table min-content now groups complete styled words and separates real optional
opportunities. Styled nowrap fragments retain internal spaces. Automatic tables
reserve indivisible column minima; captions, backgrounds and outlines use the
resulting grid width. Preferred cell widths can shrink above their intrinsic minima,
and additional space grows automatic columns proportionally. The broader table
algorithm remains partial. The frozen synthetic performance table preserves its
seven native pages, full words, fonts and resources while column allocation changes
wrapping within existing rows. A diagnostic Chromium rendering has nine pages;
the previous native output also has seven. That existing disagreement remains open.

Uniform opaque solid square CSS borders retain the existing stroke pipeline with
its raster boundary bias. Local widget appearances retain their own placement.
Complete ordinary PDF comparisons admit only reviewed border-origin changes,
plus separately recorded geometry changes in the two frozen synthetic table renders.
All other ordinary workloads require complete byte equality. The complete new
nine-page fixture retains all difficult cases and the unchanged per-page threshold.

The first excluded pilot exposed avoidable trailing-line reverse/split/rebuild
work and empty-set table allocation checks. The candidate adds a direct ordinary
tail path and an allocation path for entirely automatic columns. All 21 complete
ordinary outputs remain identical before and after these optimizations. The source
and original report remain excluded diagnostics. Supported-runtime results below
cover the final source; eprof timings never enter acceptance.

Focused coverage extends existing parser, validator, style, layout, geometry,
writer and HTML tests. Public raw/reference PDFs retain every link rectangle and
initial field value in static and interactive modes. Feature captures retain
1/10/100 instances and 1/10/50 nesting depths, including ignored own backgrounds
and unused templates whose resolvers must never run. No ExUnit file was added.

The 100-instance interactive proof retains every original positioned span unchanged
at the start of its flattened page. Flattening appends field appearances to page
content, which can interrupt a word spanning two pages in source-order extraction.
Word counts therefore use the original page content, while every flattened initial
field value is checked separately. Complete raw/reference PDFs and all URI regions
still match exactly. Five early feature reports remain excluded after strengthening
this proof; final reports include the benchmark-source hash.

The first 40 complete source captures remain excluded after profiling found
avoidable whitespace regex work. The next 42 captures exposed a repeatable
Elixir 1.20 stock-sticker slowdown of 0.480ms, or 4.183%, despite fewer reductions
and a byte-identical PDF. Restoring individual previous modules isolated the
change to Layout. Selective function counters and GC traces are excluded
diagnostics; copying traced return values distorted the first trace, so those
numbers never enter acceptance. Removing both later whitespace shortcuts
restored stock-sticker timing. Removing only one did not. Returning unchanged
line tails and a different byte-check order did not remove the slowdown.

The final candidate confines the byte-edge shortcuts to table intrinsic-width
measurement and table line layout. Only the validated table-cell layout path marks its constructed private inline
runs. Ordinary inline-run construction retains its previous function and arguments. Ordinary line trimming keeps the original shared
regex path; all callers still use one whitespace helper and exactly the same
ASCII whitespace semantics. No caller-supplied flag controls the shortcut. The
context flag stays internal and never changes glyphs, geometry or PDF resources.
Complete ordinary outputs remain identical to the prior B39 candidate. Final
source captures below determine acceptance; excluded mixed-module or compiled
variant measurements cannot establish completion.

Two discarded variants propagated table context through every inline-run builder.
Those variants improved tables but added ordinary paragraph or sticker costs.
The accepted implementation tags only runs already constructed for table-cell
layout. A source-frozen, isolated 61-sample stage repeat measured stock-sticker
changes of -1.570% on Elixir 1.19 and +0.211% on 1.20, with fewer reductions on
both. Inline paragraph changes were +0.862% and +0.431%, below the earlier
repeatable cost; final root captures below retain the longer follow-up evidence.
Table timing improved 2.538% and 5.384%, and full-font tables improved 4.512% and
6.239%. The stage had 1,285 passing unit tests, 100% coverage, nine-page soft-break
parity and the existing inline-word fixture parity. All 21 ordinary complete outputs
match the pre-optimization B39 source. Formatting occurred before the source-frozen
repeat; earlier source or mixed-module trials remain excluded diagnostics.

The final Elixir 1.19 sticker follow-up has +2.332% elapsed time, or 0.276ms,
while the 31-sample full-suite capture has -0.877%. Three further independent,
source-frozen 61-sample repeats have +0.124%, -0.082% and -0.739%, with matching
complete PDFs, all source hashes and the unchanged benchmark. The positive
follow-up is retained among the 42 official reports; it is not replaced by a
favorable repeat. The supplemental repeats are bundled in
`.quality/performance/diagnostic-captures/batch-39-final-stock-1.19-repeats/reports.json`.
Those uninstrumented measurements do not demonstrate a repeatable sticker
slowdown. Elixir 1.20 has +0.176% in the full suite and +0.138% in the longer
follow-up. The earlier repeated +4.183% Elixir 1.20 regression is removed.

Final matrix `.quality/logs/20261009T145815Z-3608538/` passes Elixir 1.19.5/OTP 28 and Elixir 1.20.2/OTP 29, with 1,285 unit tests, 100% coverage, 122 browser tests and zero warnings. The exact host Chromium command passes all 122 tests.

Forty-two final-source reports are consolidated under `.quality/performance/results/1.1.0-batch-39/reports.json`. Twelve retain fee6c4d, previous B38 and candidate in both revision orders on CPUs 4,5. Twelve feature captures retain 1/10/100 instances in static and interactive modes; six nesting captures retain depths 1/10/50. Two comparisons alternate 31 fresh-process samples on all 21 ordinary operations. Ten targeted follow-ups retain 61 samples per revision for invoice, table, full-font table inline and stock-sticker workloads. All 78 source hashes match each complete current/previous revision; the fixed baseline retains its 73 source files. Input/options/assets, thresholds and the fixed baseline remain unchanged. Direct extraction comparisons use one frozen PDF per pair. Legacy gate extraction timings use each render revision's generated PDF and are not used to claim identical-PDF extractor changes.

| Runtime and ordinary workload | Candidate ms | Previous ms | Time change | Extra reductions | Reduction change |
| --- | --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 26.824 | 26.895 | -0.264% | -4,593 | -0.212% |
| 1.19 fixture statement_012.html | 26.252 | 26.142 | +0.421% | -5,958 | -0.282% |
| 1.19 fixture multi_page_report_012.html | 28.680 | 28.629 | +0.178% | -6,112 | -0.258% |
| 1.19 fixture purchase_order.html | 39.788 | 40.018 | -0.575% | -16,074 | -0.485% |
| 1.19 fixture material_requisition.html | 54.088 | 57.865 | -6.527% | -97,258 | -2.309% |
| 1.19 fixture government_application_form.html | 40.606 | 40.599 | +0.017% | -1,520 | -0.051% |
| 1.19 fixture stock_sticker.html | 12.211 | 12.319 | -0.877% | -815 | -0.062% |
| 1.19 fixture trim_card.html | 50.376 | 51.004 | -1.231% | -20,721 | -0.605% |
| 1.19 100-row HTML render | 147.235 | 148.037 | -0.542% | -254,556 | -1.993% |
| 1.19 100-row uncompressed full-font render | 130.593 | 137.671 | -5.141% | -264,578 | -2.183% |
| 1.19 100-row PDF text extraction | 67.131 | 67.470 | -0.502% | -436 | -0.009% |
| 1.19 100-row PDF visual text extraction | 69.083 | 69.147 | -0.093% | +80 | +0.002% |
| 1.19 repeated text layout | 26.385 | 26.523 | -0.520% | -1,120 | -0.036% |
| 1.19 long paragraph layout | 50.627 | 62.366 | -18.823% | -11,333 | -0.186% |
| 1.19 five repeated PNG images | 13.572 | 13.469 | +0.765% | +46 | +0.004% |
| 1.19 ten repeated 512px PNG images | 328.936 | 328.968 | -0.010% | +1,487 | +0.010% |
| 1.19 800-word inline paragraph | 42.976 | 43.163 | -0.433% | -4,502 | -0.111% |
| 1.19 160 table headings | 54.025 | 56.460 | -4.313% | -79,991 | -1.646% |
| 1.19 4,000 boxes on one page | 14.869 | 14.811 | +0.392% | -84 | -0.004% |
| 1.19 100 ordinary Text-field fill | 394.841 | 394.295 | +0.138% | -342 | -0.001% |
| 1.19 100 ordinary Text-field flatten | 73.873 | 73.925 | -0.070% | -511 | -0.010% |
| 1.20 fixture invoice_012.html | 26.396 | 26.681 | -1.068% | -4,324 | -0.207% |
| 1.20 fixture statement_012.html | 25.714 | 26.093 | -1.452% | -6,749 | -0.329% |
| 1.20 fixture multi_page_report_012.html | 27.565 | 27.922 | -1.279% | -8,901 | -0.392% |
| 1.20 fixture purchase_order.html | 38.560 | 38.967 | -1.044% | -12,047 | -0.380% |
| 1.20 fixture material_requisition.html | 50.630 | 55.733 | -9.156% | -111,123 | -2.756% |
| 1.20 fixture government_application_form.html | 39.813 | 39.730 | +0.209% | -3,037 | -0.106% |
| 1.20 fixture stock_sticker.html | 11.400 | 11.380 | +0.176% | -1,083 | -0.082% |
| 1.20 fixture trim_card.html | 48.262 | 48.920 | -1.345% | -18,864 | -0.567% |
| 1.20 100-row HTML render | 132.184 | 140.940 | -6.213% | -250,373 | -2.175% |
| 1.20 100-row uncompressed full-font render | 119.479 | 127.242 | -6.101% | -253,787 | -2.334% |
| 1.20 100-row PDF text extraction | 64.360 | 64.289 | +0.110% | -500 | -0.011% |
| 1.20 100-row PDF visual text extraction | 65.960 | 65.917 | +0.065% | +376 | +0.008% |
| 1.20 repeated text layout | 26.121 | 26.239 | -0.450% | -1,198 | -0.039% |
| 1.20 long paragraph layout | 43.733 | 43.653 | +0.183% | -1,298 | -0.022% |
| 1.20 five repeated PNG images | 13.441 | 13.442 | -0.007% | -16 | -0.001% |
| 1.20 ten repeated 512px PNG images | 325.882 | 325.010 | +0.268% | -2,886 | -0.020% |
| 1.20 800-word inline paragraph | 39.930 | 40.347 | -1.034% | -5,932 | -0.148% |
| 1.20 160 table headings | 53.308 | 55.878 | -4.599% | -82,891 | -1.741% |
| 1.20 4,000 boxes on one page | 12.322 | 12.381 | -0.477% | +476 | +0.034% |
| 1.20 100 ordinary Text-field fill | 388.322 | 389.034 | -0.183% | -4,720 | -0.008% |
| 1.20 100 ordinary Text-field flatten | 67.259 | 67.210 | +0.073% | -452 | -0.010% |

Changed ordinary PDFs retain their full output hashes and proof artifacts. Existing fixtures preserve exact positioned text, page geometry, field/link dictionaries, resources, and all operators except tiny border origins. The two synthetic table renders retain each page's independent Poppler word sequence, nonspace glyph/font/paint sequence, text colors, all object dictionaries except stream length, and nontext operators. Their wrapping positions are recorded for review rather than silently equated. Unchanged outputs require complete SHA-256 equality.

Elixir 1.19, 100 static instances: 103.359ms versus authored 97.855ms, +444,791 reductions. First render 176.704ms; sampled VM peak 85,201,248 bytes; output 221,247 bytes. Every complete raw/reference PDF, URI region and initial field value agrees.

Elixir 1.19, 100 interactive instances: 174.472ms versus authored 159.103ms, +447,204 reductions. First render 253.234ms; sampled VM peak 85,787,936 bytes; output 471,512 bytes. Every complete raw/reference PDF, URI region and initial field value agrees.

Elixir 1.20, 100 static instances: 104.652ms versus authored 94.101ms, +390,108 reductions. First render 170.700ms; sampled VM peak 84,750,448 bytes; output 221,247 bytes. Every complete raw/reference PDF, URI region and initial field value agrees.

Elixir 1.20, 100 interactive instances: 165.759ms versus authored 150.848ms, +416,747 reductions. First render 235.709ms; sampled VM peak 84,392,928 bytes; output 471,512 bytes. Every complete raw/reference PDF, URI region and initial field value agrees.

The 61-sample follow-ups retain the same inputs, source hashes and full-output proofs.

| Runtime and workload | Candidate ms | Previous ms | Time change | Extra reductions | Reduction change |
| --- | --- | --- | --- | --- | --- |
| 1.19 100-row HTML render | 143.520 | 146.268 | -1.879% | -257,200 | -2.014% |
| 1.19 100-row uncompressed full-font render | 129.596 | 137.896 | -6.019% | -273,913 | -2.258% |
| 1.19 800-word inline paragraph | 42.653 | 42.606 | +0.110% | -6,065 | -0.149% |
| 1.19 fixture invoice_012.html | 26.799 | 26.912 | -0.420% | -4,708 | -0.217% |
| 1.19 fixture stock_sticker.html | 12.111 | 11.835 | +2.332% | -628 | -0.047% |
| 1.20 100-row HTML render | 138.780 | 146.839 | -5.488% | -249,684 | -2.168% |
| 1.20 100-row uncompressed full-font render | 119.657 | 127.932 | -6.468% | -253,750 | -2.335% |
| 1.20 800-word inline paragraph | 39.663 | 39.540 | +0.311% | -5,108 | -0.128% |
| 1.20 fixture invoice_012.html | 26.218 | 26.144 | +0.283% | -3,986 | -0.191% |
| 1.20 fixture stock_sticker.html | 11.581 | 11.565 | +0.138% | -426 | -0.032% |

Three narrow UA-default records become implemented; five broader wbr/nobr records become partial. Counts are 2,254 implemented, 4,691 partial, 2,204 planned, 421 printable exclusions and 7,697 non-printing records. The audit retains all 17,267 IDs. Global/parser/integration work, full CSS text/shaping and general table sizing remain open. Cumulative milestone performance and release acceptance remain open.

The final inventory audit corrected eight stale script recommendations from B28.
Six mixed/static data records were incorrectly left excluded despite their
implemented data-block behavior; they are now partial, with their original IDs
and source links. Classic JavaScript execution and runtime script preparation
remain excluded and have explicit execution recommendations. This repairs the
print-view audit without introducing execution support or hiding unfinished work.

## Batch 40: heading offsets and semantic bookmarks

HTML headingoffset and boolean headingreset affect automatically generated PDF
bookmark levels. The canonical validator parses offsets with the existing HTML
integer parser and numeric limit. Each inclusive HTML ancestor contributes its
parsed nonnegative value until an inclusive reset; invalid and negative values
contribute zero. The computed h1-h6 level caps at the specification's fixed nine.
The author-conformance range 0-8 is distinct from the runtime parsing algorithm.
Qualified and foreign attributes neither contribute nor reset context. Numeric
limit failures retain the shared diagnostic tuple, attribute source and available
line/column information. Advanced styled trees validate heading-level metadata;
supplied DOM flags are replaced by values derived from validated attributes.

Context flows through the existing selector-ID traversal without a separate
ancestor scan. Ordinary text does not inherit heading metadata. Typography,
positions, fields and link regions remain unchanged; complete raw/reference PDFs
verify this. Explicit caller outlines retain their existing precedence. Block,
anonymous-flow, flex/grid, inline and atomic headings preserve computed levels.
Wrapped inline fragments share one internal identity, preventing duplicate
bookmarks. Distinct headings with identical titles remain separate. Hidden title
descendants and unused templates contribute no bookmark text; inert text prints.
Internal identities never reach PDF output. Existing ExUnit files are extended.

The official heading fixture retains offsets, additions, resets, parsing edges,
the level-nine cap, valid custom containers, flex/grid, anonymous flow, links,
inert and hidden content, unused templates, inline and atomic headings. It passes
the unchanged 1% threshold. A separate complete mixed-inline/inline-block diagnostic
records an existing 0.75-point baseline disagreement, with 1.128% changed pixels.
Its mixed case remains in the direct metadata and complete visible-PDF tests; the
official fixture tests both display modes in separate flow lines. The later inline
baseline batch must resolve this existing disagreement before full 1.1.0 release.

The broader heading rows remain partial until namespace/foreign-document and
shadow-host composition integrate the canonical offset rules. No rendering or
release completeness is claimed for those dependencies. All original IDs and
source links are retained, including older inventory attribute links; the pinned
heading section is the primary semantic source for this batch.

Before root application, the staged ordinary suite showed three higher timing
medians: stock stickers on 1.19 at +3.163%, form flattening on 1.19 at +3.186%,
and purchase orders on 1.20 at +1.500%. All complete PDFs were identical to B39.
Three independent 61-sample VM repetitions for each operation did not reproduce
these increases. Sticker changes were +0.223%, -0.033% and +0.370%; flattening
changes were +0.467%, -0.794% and -0.468%; purchase-order changes were -0.219%,
+0.743% and -0.489%. All nine supplemental reports retain the same source, input
and output hashes under `.quality/performance/diagnostic-captures/batch-40-stage-repeats/`.
The original higher measurements remain available rather than being replaced.
Final root measurements are recorded below and reviewed separately.

Final root captures preserve all 21 ordinary outputs byte for byte on each runtime.
The 1.19 first-suite stock increase of +3.153% did not persist in its 61-sample
follow-up, which measured -0.580%; 1.20 measured +0.087%. The 1.19 inline paragraph
changed from +1.170% to +0.371% in its longer follow-up, while 1.20 measured -0.341%.
Table follow-ups measured -0.817% and +0.227%; full-font tables measured -1.895%
and +0.142%; invoice follow-ups measured +0.442% and +0.217%, on 1.19 and 1.20.
All reductions remained close to B39 and the unchanged performance gates passed.

Two remaining 1.19 first-suite increases received three further independent
61-sample VM repetitions. The 4,000-box operation, initially +2.098%, repeated at
+0.287%, +0.007% and +0.570%. Statements, initially +1.002%, repeated at +0.720%,
+1.320% and +0.359%, with reduction changes +0.007%, +0.044% and +0.008%.
The larger statement repetition is retained. These measurements did not establish
a consistent increase beyond observed timing variation. All original inputs,
options, assets, output hashes and source hashes agree. The six supplemental
reports are bundled under `.quality/performance/diagnostic-captures/batch-40-final-repeats/reports.json`.
No baseline, tolerance or threshold was changed.

Root matrix `.quality/logs/20261009T155135Z-3698952/` passes both supported runtimes with 1,298 unit tests, 100% coverage, 123 browser tests and zero warnings. The exact host command passes all 123 browser tests.

Forty-two final-source reports are consolidated under `.quality/performance/results/1.1.0-batch-40/reports.json`. Twelve retain fixed fee6c4d, exact B39 and candidate in both revision orders. Twelve feature captures retain 1/10/100 heading groups in static and interactive modes; six retain ancestor depths 1/10/50. Two alternate 31 fresh-process samples over all 21 ordinary operations, and ten repeat invoice, table, full-font table, inline paragraph and stock sticker with 61 samples. Complete ordinary outputs must agree byte for byte. Feature comparisons retain complete automatic-outline PDFs versus equivalent reference artwork and independently verified explicit outline trees, plus every field value, URI region and flattened text prefix. Cold timing is exploratory; sampled VM peaks exclude native and short-lived peaks. All source, input, asset, benchmark and option hashes are retained. Baselines and thresholds are unchanged.

| Runtime and ordinary workload | Candidate ms | B39 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 27.146 | 26.950 | +0.727% | +0.023% |
| 1.19 fixture statement_012.html | 24.703 | 24.458 | +1.002% | +0.023% |
| 1.19 fixture multi_page_report_012.html | 28.731 | 28.608 | +0.430% | +0.059% |
| 1.19 fixture purchase_order.html | 39.692 | 39.459 | +0.590% | +0.022% |
| 1.19 fixture material_requisition.html | 53.289 | 53.235 | +0.101% | +0.018% |
| 1.19 fixture government_application_form.html | 40.592 | 40.779 | -0.459% | +0.015% |
| 1.19 fixture stock_sticker.html | 12.137 | 11.766 | +3.153% | +0.036% |
| 1.19 fixture trim_card.html | 49.331 | 49.149 | +0.370% | +0.031% |
| 1.19 100-row HTML render | 144.161 | 145.343 | -0.813% | +0.568% |
| 1.19 100-row uncompressed full-font render | 126.789 | 128.953 | -1.678% | +0.535% |
| 1.19 100-row PDF text extraction | 66.465 | 66.218 | +0.373% | +0.009% |
| 1.19 100-row PDF visual text extraction | 67.438 | 67.451 | -0.019% | +0.007% |
| 1.19 repeated text layout | 25.758 | 25.766 | -0.031% | -0.001% |
| 1.19 long paragraph layout | 49.876 | 49.981 | -0.210% | +0.002% |
| 1.19 five repeated PNG images | 13.525 | 13.569 | -0.324% | -0.003% |
| 1.19 ten repeated 512px PNG images | 327.134 | 326.599 | +0.164% | -0.010% |
| 1.19 800-word inline paragraph | 43.925 | 43.417 | +1.170% | +0.024% |
| 1.19 160 table headings | 53.007 | 53.402 | -0.740% | -0.008% |
| 1.19 4,000 boxes on one page | 15.185 | 14.873 | +2.098% | +0.007% |
| 1.19 100 ordinary Text-field fill | 397.500 | 396.349 | +0.290% | -0.003% |
| 1.19 100 ordinary Text-field flatten | 71.043 | 71.038 | +0.007% | +0.007% |
| 1.20 fixture invoice_012.html | 26.411 | 26.500 | -0.336% | -0.014% |
| 1.20 fixture statement_012.html | 26.039 | 25.963 | +0.293% | +0.004% |
| 1.20 fixture multi_page_report_012.html | 28.122 | 28.206 | -0.298% | -0.042% |
| 1.20 fixture purchase_order.html | 38.819 | 38.487 | +0.863% | +0.018% |
| 1.20 fixture material_requisition.html | 45.143 | 44.995 | +0.329% | +0.016% |
| 1.20 fixture government_application_form.html | 39.869 | 40.015 | -0.365% | +0.004% |
| 1.20 fixture stock_sticker.html | 11.565 | 11.510 | +0.478% | -0.009% |
| 1.20 fixture trim_card.html | 50.031 | 50.079 | -0.096% | +0.018% |
| 1.20 100-row HTML render | 133.945 | 133.929 | +0.012% | +0.066% |
| 1.20 100-row uncompressed full-font render | 121.346 | 120.893 | +0.375% | +0.074% |
| 1.20 100-row PDF text extraction | 65.778 | 65.741 | +0.056% | -0.001% |
| 1.20 100-row PDF visual text extraction | 67.868 | 67.309 | +0.830% | -0.001% |
| 1.20 repeated text layout | 25.774 | 25.712 | +0.241% | +0.007% |
| 1.20 long paragraph layout | 43.679 | 43.654 | +0.057% | +0.005% |
| 1.20 five repeated PNG images | 13.479 | 13.448 | +0.231% | -0.000% |
| 1.20 ten repeated 512px PNG images | 323.019 | 322.630 | +0.121% | +0.002% |
| 1.20 800-word inline paragraph | 40.242 | 40.062 | +0.449% | +0.007% |
| 1.20 160 table headings | 54.687 | 54.150 | +0.992% | -0.002% |
| 1.20 4,000 boxes on one page | 12.161 | 12.323 | -1.315% | +0.000% |
| 1.20 100 ordinary Text-field fill | 390.539 | 389.980 | +0.143% | +0.006% |
| 1.20 100 ordinary Text-field flatten | 68.153 | 68.265 | -0.164% | -0.014% |

Feature costs are measured separately from ordinary documents. The reference uses equivalent artwork and verified explicit outlines, so its timing is a correctness comparison, not an isolated estimate of attribute overhead. Cold captures include initial process work. VM peak samples use a 10 ms interval and exclude native allocations and short-lived peaks.

| Runtime | Groups | Ancestor depth | Mode | Cold ms | Raw median ms | Reference median ms | Maximum sampled VM MiB |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1.19 | 1 | 2 | interactive | 97.759 | 17.360 | 17.379 | 61.18 |
| 1.20 | 1 | 2 | interactive | 93.598 | 17.428 | 17.316 | 61.41 |
| 1.19 | 1 | 2 | static | 85.399 | 15.991 | 15.930 | 61.02 |
| 1.20 | 1 | 2 | static | 82.634 | 15.566 | 15.741 | 59.57 |
| 1.19 | 10 | 2 | interactive | 115.134 | 30.918 | 30.132 | 65.90 |
| 1.20 | 10 | 2 | interactive | 110.954 | 29.896 | 29.632 | 74.81 |
| 1.19 | 10 | 2 | static | 98.900 | 24.938 | 24.801 | 67.64 |
| 1.20 | 10 | 2 | static | 92.935 | 24.846 | 24.536 | 66.54 |
| 1.19 | 100 | 2 | interactive | 267.987 | 178.124 | 193.544 | 82.12 |
| 1.20 | 100 | 2 | interactive | 253.227 | 173.803 | 188.281 | 81.72 |
| 1.19 | 100 | 2 | static | 196.713 | 130.179 | 129.595 | 81.34 |
| 1.20 | 100 | 2 | static | 186.917 | 121.926 | 120.280 | 81.18 |
| 1.19 | 1 | 1 | static | 83.336 | 15.848 | 15.772 | 61.03 |
| 1.20 | 1 | 1 | static | 83.212 | 15.385 | 15.638 | 59.57 |
| 1.19 | 1 | 10 | static | 85.275 | 16.752 | 16.739 | 62.85 |
| 1.20 | 1 | 10 | static | 84.178 | 16.913 | 16.861 | 61.39 |
| 1.19 | 1 | 50 | static | 90.366 | 24.463 | 24.096 | 66.46 |
| 1.20 | 1 | 50 | static | 90.876 | 24.073 | 23.670 | 66.64 |

Two attribute records become partial; eight existing partial heading records gain tested ordinary HTML offset behavior. Counts are 2,254 implemented, 4,693 partial, 2,202 planned, 421 printable exclusions and 7,697 non-printing records. All 17,267 original IDs remain. Other 1.1.0 features, cumulative performance and release acceptance remain open.

## Batch 41: inline block baselines and forced-line widths

Inline blocks use the last normal-flow line's baseline, including wrapped and
forced lines, rather than their font size. Empty normal flow and non-visible
overflow use the bottom margin edge. Declared content height does not move an
existing flow baseline. The surrounding line combines each block's ascent and
descent with the parent strut, preserving room below empty or clipped blocks.
Margins, padding and borders enter the measurement. Relative paint offsets and
absolute descendants do not select or move the flow baseline. Empty text controls
and buttons retain their existing intrinsic line struts.

Prepared width, height, baseline and child lines are attached to atomic runs and
reused for line sizing and painting. The normal-flow measurement copy excludes
positioned descendants and visual offsets without changing artwork. Internal
baseline records require the current measurement identity and never enter PDF
output. Caller-supplied private flags cannot enable them. Existing validator-owned
resource accounting, including drawing construction attempts during measurement,
remains unchanged. There is no new cache, budget exemption or public input rule.

Automatic multiline inline blocks use their widest forced line for intrinsic
width. This reuses the existing line builder with wrapping and block-height
measurements disabled. Nested atomic blocks and preserved/forced blank lines
retain correct trailing text placement. Existing ExUnit files are extended.

The complete B40 mixed-heading diagnostic is retained intact as page one of
`css_inline_block_baselines.html`. The added page retains single/multiline, tall,
empty, overflow-hidden, block-child, relative, nested and blank-line cases.
Initial staged comparisons passed at 0.7271% and 0.5429% changed pixels with the
unchanged 1% threshold and identical per-page word counts. Both visible pages,
complete PDFs with URI regions and editable fields, and retained original spans
after fill/flatten are checked.

The broad inline formatting inventory remains partial. Other vertical-align
values, full shrink-to-fit behavior, inline-flex/grid baseline integration,
writing modes and the remaining CSS/HTML dependencies are not completed by this
batch. The earlier mixed-heading baseline disagreement is resolved; the direct
30-point MathML ink-metric disagreement and overall 1.1.0 release work remain open.

The first staged implementation passed all content and parity checks but caused
a repeatable Elixir 1.19 long-paragraph timing regression. Its first ordinary
median rose +25.332%; three independent 61-sample VM repetitions measured
+24.452%, +23.925% and +24.978%. Reductions changed only about +0.16% to +0.18%.
Raw samples had fast runs near 50 ms and slower runs near 63 ms, with the candidate
entering the slower GC pattern more frequently. Diagnostic GC tracing changed
the heap/timing distribution, so traced timings are excluded from acceptance.

Moving measurement bookkeeping into a named helper avoided capturing an extra
flag in the ordinary line-rendering closure. The same features, rendering limits
and default GC settings remained. Three independent 61-sample comparisons then
measured -0.276%, -0.470% and -0.377%, with byte-identical PDFs. This supports a
heap/GC sensitivity caused by the loop change, rather than added rendering work.
The original source, two ordinary reports, three reproductions and original
quality logs remain under `.quality/performance/diagnostic-captures/batch-41-before-loop-fix/`.
The three experimental fixed-source reports remain under
`.quality/performance/diagnostic-captures/batch-41-no-tracking-scan/`.
Full quality and ordinary suites are rerun on the promoted source before root
application. No heap setting, limit, baseline or threshold was changed.

The corrected stage rerun compares all 21 ordinary operations with exact B40 on
both supported runtimes, with complete byte equality. Long paragraph timing is
+0.087% on Elixir 1.19 and -0.402% on Elixir 1.20. The first corrected 1.19
capture shows invoice +2.318% and material requisition +2.790%. Three independent
61-sample repetitions on unchanged corrected source measure invoice +0.137%,
-0.829% and -0.727%, and material requisition +0.824%, +0.833% and +0.119%.
These increases do not persist beyond ordinary variation. Their reductions
remain within +0.022%. Other corrected-stage ordinary increases stay below 1%.
The full reports and six repetitions remain under ignored
`.quality/performance/diagnostic-captures/batch-41-stage/` and
`batch-41-stage-repeats/`; all source hashes and original timing samples remain.
The corrected staged suite passes 1,306 unit tests, 100% coverage and all 124
browser cases. Final root captures are assessed separately below.

Final-root long-paragraph medians change -0.223% on Elixir 1.19 and +0.686% on
Elixir 1.20. The final 61-sample table/full-font/inline/invoice/stock comparisons
all change by less than 0.6%; stock moves from initial +2.543%/+1.278% to
-0.512%/+0.104%. Reductions remain within +0.081% in those longer comparisons.
All 21 complete ordinary outputs agree byte for byte on both runtimes.

The two-order fixed-fee6c4d captures are retained for the cumulative release audit.
Most current rendering operations are faster and use fewer reductions. The
4,000-box writer operation still uses about +1.74% reductions on Elixir 1.19 and
+2.78% on Elixir 1.20, with timing increases +1.24% to +1.37% and +0.25% to +0.87%.
This earlier cumulative overhead is not caused by Layout-only B41. It remains
part of the open release-performance review. Legacy fixed-baseline extraction
uses each revision's generated PDF, whereas direct B40/B41 extraction compares
one frozen input. Legacy extraction differences cannot establish an extractor
regression on identical input. No baseline, resource ceiling or threshold is reset.

Six independent final-root 61-sample repetitions preserve exact B40/current
sources, inputs and outputs. Purchase-order timing changes +0.157%, -0.020%
and +1.620%; the largest remains recorded, but the increase is not consistent.
Its reduction changes stay within +0.063%. Direct frozen-PDF extraction changes
-0.213%, -0.294% and -0.198%, with reduction changes within 0.007%.
These supplemental reports are consolidated under
`.quality/performance/diagnostic-captures/batch-41-final-repeats/reports.json`;
original files and SHA-256 digests are retained separately. No outlier is deleted.

Root matrix `.quality/logs/20261009T171203Z-3841052/` passes both supported runtimes with 1,306 unit tests, 100% coverage, 124 browser tests and zero warnings. The exact host command passes all 124 browser tests.

Forty-two final-source reports are consolidated under `.quality/performance/results/1.1.0-batch-41/reports.json`. Twelve retain fixed fee6c4d, exact B40 and candidate in both revision orders. Twelve feature captures retain 1/10/100 inline-block groups in static and interactive modes; six retain inline-block depths 1/10/50. Two alternate 31 fresh-process samples over all 21 ordinary operations, and ten repeat invoice, table, full-font table, inline paragraph and stock sticker with 61 samples. Complete ordinary outputs must agree byte for byte. Feature comparisons retain complete automatic-outline PDFs versus equivalent reference artwork and independently verified explicit outline trees, plus every field value, URI region and flattened text prefix. Cold timing is exploratory; sampled VM peaks exclude native and short-lived peaks. All source, input, asset, benchmark and option hashes are retained. Baselines and thresholds are unchanged.

| Runtime and ordinary workload | Candidate ms | B40 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 27.014 | 26.767 | +0.923% | +0.032% |
| 1.19 fixture statement_012.html | 26.457 | 26.481 | -0.091% | +0.016% |
| 1.19 fixture multi_page_report_012.html | 28.206 | 28.658 | -1.577% | -0.026% |
| 1.19 fixture purchase_order.html | 40.151 | 39.215 | +2.387% | +0.059% |
| 1.19 fixture material_requisition.html | 52.742 | 53.252 | -0.958% | -0.008% |
| 1.19 fixture government_application_form.html | 40.641 | 40.657 | -0.039% | -0.010% |
| 1.19 fixture stock_sticker.html | 12.137 | 11.836 | +2.543% | +0.016% |
| 1.19 fixture trim_card.html | 48.495 | 48.544 | -0.101% | +0.032% |
| 1.19 100-row HTML render | 146.544 | 146.493 | +0.035% | +0.060% |
| 1.19 100-row uncompressed full-font render | 129.738 | 129.754 | -0.012% | +0.064% |
| 1.19 100-row PDF text extraction | 66.480 | 65.631 | +1.294% | +0.005% |
| 1.19 100-row PDF visual text extraction | 67.485 | 67.548 | -0.093% | +0.002% |
| 1.19 repeated text layout | 26.404 | 26.388 | +0.061% | +0.016% |
| 1.19 long paragraph layout | 50.576 | 50.689 | -0.223% | -0.005% |
| 1.19 five repeated PNG images | 13.571 | 13.548 | +0.170% | +0.000% |
| 1.19 ten repeated 512px PNG images | 329.388 | 329.924 | -0.162% | -0.016% |
| 1.19 800-word inline paragraph | 41.598 | 41.746 | -0.355% | +0.027% |
| 1.19 160 table headings | 53.364 | 53.825 | -0.856% | +0.058% |
| 1.19 4,000 boxes on one page | 15.010 | 15.067 | -0.378% | +0.003% |
| 1.19 100 ordinary Text-field fill | 396.060 | 394.672 | +0.352% | +0.005% |
| 1.19 100 ordinary Text-field flatten | 68.104 | 67.891 | +0.314% | +0.002% |
| 1.20 fixture invoice_012.html | 26.369 | 26.529 | -0.603% | -0.012% |
| 1.20 fixture statement_012.html | 25.816 | 25.717 | +0.385% | -0.255% |
| 1.20 fixture multi_page_report_012.html | 27.806 | 28.019 | -0.760% | -0.042% |
| 1.20 fixture purchase_order.html | 38.510 | 38.466 | +0.114% | +0.039% |
| 1.20 fixture material_requisition.html | 51.267 | 51.145 | +0.239% | +0.018% |
| 1.20 fixture government_application_form.html | 39.496 | 39.431 | +0.165% | -0.004% |
| 1.20 fixture stock_sticker.html | 11.012 | 10.873 | +1.278% | +0.031% |
| 1.20 fixture trim_card.html | 47.187 | 47.580 | -0.826% | -0.024% |
| 1.20 100-row HTML render | 131.197 | 131.610 | -0.314% | +0.071% |
| 1.20 100-row uncompressed full-font render | 119.928 | 119.600 | +0.274% | +0.063% |
| 1.20 100-row PDF text extraction | 64.645 | 64.265 | +0.591% | +0.002% |
| 1.20 100-row PDF visual text extraction | 65.328 | 65.943 | -0.933% | +0.004% |
| 1.20 repeated text layout | 26.160 | 26.184 | -0.092% | +0.022% |
| 1.20 long paragraph layout | 44.164 | 43.863 | +0.686% | +0.061% |
| 1.20 five repeated PNG images | 13.370 | 13.385 | -0.112% | -0.000% |
| 1.20 ten repeated 512px PNG images | 325.464 | 325.819 | -0.109% | -0.002% |
| 1.20 800-word inline paragraph | 40.141 | 39.893 | +0.622% | +0.017% |
| 1.20 160 table headings | 52.712 | 52.683 | +0.055% | +0.129% |
| 1.20 4,000 boxes on one page | 12.472 | 12.577 | -0.835% | -0.008% |
| 1.20 100 ordinary Text-field fill | 388.302 | 388.607 | -0.078% | -0.004% |
| 1.20 100 ordinary Text-field flatten | 66.187 | 66.266 | -0.119% | +0.000% |

Feature costs are measured separately from ordinary documents. The reference uses equivalent inline artwork and verified explicit outlines. Its timing compares complete rendering paths and does not isolate baseline measurement cost. Cold captures include initial process work. VM peak samples use a 10 ms interval and exclude native allocations and short-lived peaks.

| Runtime | Groups | Inline-block depth | Mode | Cold ms | Raw median ms | Reference median ms | Maximum sampled VM MiB |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1.19 | 1 | 1 | interactive | 104.798 | 20.867 | 21.027 | 65.93 |
| 1.20 | 1 | 1 | interactive | 102.227 | 21.143 | 21.111 | 69.51 |
| 1.19 | 1 | 1 | static | 84.377 | 12.809 | 12.683 | 61.79 |
| 1.20 | 1 | 1 | static | 81.822 | 12.881 | 12.518 | 62.86 |
| 1.19 | 10 | 1 | interactive | 122.256 | 30.364 | 30.777 | 69.36 |
| 1.20 | 10 | 1 | interactive | 113.792 | 28.602 | 28.995 | 68.10 |
| 1.19 | 10 | 1 | static | 89.277 | 17.762 | 17.747 | 66.38 |
| 1.20 | 10 | 1 | static | 88.778 | 16.413 | 17.157 | 67.24 |
| 1.19 | 100 | 1 | interactive | 212.370 | 139.108 | 135.700 | 80.92 |
| 1.20 | 100 | 1 | interactive | 201.866 | 131.125 | 127.078 | 83.63 |
| 1.19 | 100 | 1 | static | 146.308 | 85.508 | 82.322 | 82.59 |
| 1.20 | 100 | 1 | static | 134.696 | 78.165 | 75.103 | 82.22 |
| 1.19 | 1 | 1 | static | 86.152 | 12.784 | 12.657 | 60.88 |
| 1.20 | 1 | 1 | static | 83.864 | 12.872 | 12.927 | 62.78 |
| 1.19 | 1 | 10 | static | 92.573 | 12.556 | 12.440 | 61.79 |
| 1.20 | 1 | 10 | static | 85.068 | 12.286 | 12.263 | 67.64 |
| 1.19 | 1 | 50 | static | 92.309 | 15.900 | 13.188 | 62.00 |
| 1.20 | 1 | 50 | static | 87.011 | 16.393 | 13.356 | 64.66 |

Related inline formatting and control inventory rows remain partial; the earlier mixed-heading baseline gap is resolved. Counts stay at 2,254 implemented, 4,693 partial, 2,202 planned, 421 printable exclusions and 7,697 non-printing records. All 17,267 original IDs remain. Full 1.1.0 completion and release acceptance remain open.

## Batch 42: native form appearance objects

HTML form creation and form fills now write appearance Form XObjects directly.
They reuse the writer's font subsetting, image decoding, graphics states,
resource dictionaries and content commands. The old path created a temporary
PDF, read its whole catalog and page tree, copied those unused objects into the
main PDF, and then wrapped its content streams as appearances. The new path
retains each active appearance and resource while removing the unused temporary
catalog, pages and duplicate content streams. Field, widget, annotation and
root order stay unchanged. Field creation accumulates lists in linear time.

Actual pages retain the existing page ceilings. Appearance frames use the
existing appearance-widget ceiling. The canonical writer validator owns both
paths and their shared drawing/font/input rules; the consuming writer's old
page/box checks are removed. Serialized resource bodies use the existing strict
PDF tokenizer/parser and validator before the incremental writer receives
structured objects. Reader token, depth, value, input-byte and decoded-byte
budgets remain shared. Object allocation and complete output bytes retain their
existing limits. There is no threshold increase, input-validation bypass or
unchecked serialized-object API.

Existing ExUnit files now cover exact direct/legacy appearance resource graphs,
embedded and builtin fonts, alpha PNG, CMYK JPEG tint functions, opacity,
glyph groups, empty frames, page/appearance budget separation, exact object
capacity boundaries and malformed serialized resource diagnostics. Tests render,
fill and flatten 1, 100 and 1,000 text controls under the default limits.

Separate mixed-control captures contain text, passwords, checkboxes, radios,
single choices and multiline text, including readonly and disabled controls.
Each field value, flag, widget, normal/background or Off/on appearance, label
and URI region is checked. Initial flattened text and geometry match an
independent static drawing path. Filling retains expected values and all original
page spans. Password values remain masked in appearances. All supported B41
outputs preserve their complete reachable PDF graphs and raw stream bytes.
Only verified unused temporary objects may disappear; existing input orphans
must remain unchanged. Other ordinary outputs must be byte-identical.

The earlier 1,000-control interactive failure is resolved without raising page
limits. A mixed document has 37 actual pages and 2,000 appearance frames.
B41 rejected the frames against the 1,000-page ceiling; the direct appearance
path accepts them under the existing appearance/object/byte budgets. This is a
capacity correction. A failed previous rendering cannot supply a successful
speed comparison.

Initial staged government-form medians improve 2.572% on Elixir 1.19 and 4.451%
on Elixir 1.20; 100-field fill improves 8.896% and 1.138%. All 21 ordinary outputs
are checked in both runtimes. The initial Elixir 1.20 table-heading increase
of 1.175% becomes 0.319%, 0.085% and 0.062% in three independent 61-sample runs.

Elixir 1.19 frozen-input flatten initially increases 1.219%, with three longer
median differences of 1.692%, 2.045% and 0.441%. No sample is discarded.
Three calls per revision have identical counts across 224 library functions,
3,754,776 calls in total, and the existing flatten/reader validation paths are
unchanged. Reductions differ by at most 0.013%; each sample has one minor GC.
Paired mean timing differences are 0.952%, 0.007% and 0.557%. Exploratory paired
bootstrap intervals include zero in each run, with ranges -0.067% to 1.977%,
-1.250% to 1.330%, and -0.675% to 1.822%. These measurements support ordinary
variation rather than added flatten work. The larger median differences remain
recorded, and final captures are assessed separately below. The profiling
instrumentation is excluded from acceptance timings. Heap settings and
performance gates are unchanged.

Broader control ownership, label/fieldset/legend semantics, constraints,
listbox/multiple select rendering, native themes, fractional widget raster
parity, SVG, MathML and embedded-document work remain unfinished. Resolving
appearance allocation does not mark those standards rows complete. The direct
30-point MathML ink-metric disagreement remains open; equivalent CSS glyph
rows already pass exactly after batch 11.

### Final performance review

Final frozen-input government-form creation improves 2.371% on Elixir 1.19 and
4.964% on 1.20. The 61-sample ordinary fill improves 9.864% and 1.210%.
Ordinary flatten differences are -0.999% and +0.057%. Rendering 100 mixed
interactive controls improves 32.549% and 39.306%, with medians 60.200 ms and
52.555 ms. The 1,000-control interactive medians are 657.980 ms and 629.132 ms.
Both outputs have 37 pages, 5,089 objects and 1,403,823 bytes. Whole-VM sampled
peaks are 166.30 MiB and 185.30 MiB, subject to the sampling limits below.

The unchanged five-sample legacy captures retain their largest PNG timing
increase, 46.807% on Elixir 1.20. The previous samples include 224-230 ms and
320 ms results; the candidate includes 228 ms and 312-342 ms results. Both
revisions have fast and slow timing modes. The 31-sample frozen-input comparison
is +0.064%, with reductions +0.005%. Three independent 61-sample repeats below
retain the same input, source, assets and exact output. They do not reproduce
the large increase. No samples, timing modes or legacy results are removed.

The 100-control static medians initially increase 1.913% and 1.634%. Three longer
runs per runtime retain all samples. Elixir 1.19 median differences stay near
+0.5%, while paired mean differences are +0.419%, +2.000% and -1.736%.
Exploratory paired log-ratio bootstrap intervals are -2.495% to +3.361%,
-1.452% to +5.540%, and -5.697% to +2.325%. Each includes zero. Candidate-slower
pairs are 33/61, 35/61 and 30/61. The variation between fast and slow samples
therefore prevents attributing the small median increase to added work.
Reduction differences remain within 0.023% across both runtimes. Elixir 1.20
longer median differences are negative in all three runs. The larger initial
medians remain recorded; no performance thresholds are changed.

Fifteen supplemental reports are consolidated into ignored diagnostic bundles,
with original report bytes and digests retained. The nine ordinary captures and
six control captures pass exact source, script, input, asset and output checks.
Paired statistics use 10,000 resamples with seed 42 and are exploratory evidence,
not a replacement for the original median measurements.

| Supplemental runtime and workload | Candidate ms | B41 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.20 1, fixture material_requisition.html | 53.185 | 53.021 | +0.309% | +0.004% |
| 1.20 1, ten repeated 512px PNG images | 322.062 | 322.834 | -0.239% | -0.010% |
| 1.19 1, fixture trim_card.html | 50.038 | 50.254 | -0.430% | -0.019% |
| 1.20 2, fixture material_requisition.html | 53.371 | 53.586 | -0.401% | +0.004% |
| 1.20 2, ten repeated 512px PNG images | 323.511 | 322.601 | +0.282% | -0.000% |
| 1.19 2, fixture trim_card.html | 49.045 | 49.157 | -0.228% | -0.018% |
| 1.20 3, fixture material_requisition.html | 53.435 | 53.245 | +0.357% | -0.000% |
| 1.20 3, ten repeated 512px PNG images | 321.517 | 320.776 | +0.231% | +0.001% |
| 1.19 3, fixture trim_card.html | 48.317 | 48.402 | -0.176% | +0.002% |
| 1.19 1, 100 mixed static controls | 44.453 | 44.240 | +0.481% | +0.015% |
| 1.20 1, 100 mixed static controls | 34.002 | 34.023 | -0.062% | +0.004% |
| 1.19 2, 100 mixed static controls | 43.481 | 43.232 | +0.576% | +0.019% |
| 1.20 2, 100 mixed static controls | 33.812 | 33.856 | -0.130% | +0.023% |
| 1.19 3, 100 mixed static controls | 43.415 | 43.219 | +0.454% | -0.014% |
| 1.20 3, 100 mixed static controls | 34.045 | 34.061 | -0.047% | -0.006% |

The fixed fee6c4d comparison remains a separate, unfinished release audit.
The 4,000-box workload has cumulative reduction increases of 1.758% and 1.765%
on Elixir 1.19, and 2.791% and 2.786% on 1.20. Its timing differences are +0.534%
and +1.709%, and +0.064% and +1.805%. Elixir 1.20 table-heading timings increase
6.377% and 3.822%, despite reductions improving 16.554% and 16.694%; inline text
has one +2.372% timing result with reductions improving 1.269%. B42's longer
comparisons against B41 do not show those increases, so these cumulative cases
still require milestone-wide analysis. Passing the existing guardrails does
not close that audit or make 1.1.0 ready for release.

Root matrix `.quality/logs/20261009T200450Z-3995065/` passes both supported runtimes with 1,312 unit tests, 100% coverage, 124 browser comparisons and zero warnings. The exact host command passes all 124 comparisons. Existing parity thresholds and fixtures are retained because active artwork is unchanged.

Forty-six reports are consolidated under ignored `.quality/performance/results/1.1.0-batch-42/reports.json`, with original bytes and SHA-256 digests retained separately. Twelve compare fixed fee6c4d, exact B41 and candidate in both orders; eighteen measure mixed controls at 1/100/1,000 controls or nesting depths 1/10/50; two alternate 31 fresh-process samples over all 21 ordinary operations; fourteen repeat table, full-font, inline, invoice, stock, fill and flatten with 61 samples. Feature captures retain previous/candidate cold results and warmed/memory comparisons when both revisions succeed. Previous 1,000-control interactive failures remain recorded without invented timing comparisons.

Final ordinary workloads:

| Runtime and workload | Candidate ms | B41 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 27.301 | 27.281 | +0.073% | +0.011% |
| 1.19 fixture statement_012.html | 26.198 | 26.448 | -0.945% | -0.027% |
| 1.19 fixture multi_page_report_012.html | 28.388 | 28.258 | +0.460% | -0.019% |
| 1.19 fixture purchase_order.html | 39.414 | 39.461 | -0.119% | +0.010% |
| 1.19 fixture material_requisition.html | 52.957 | 52.840 | +0.221% | +0.031% |
| 1.19 fixture government_application_form.html | 39.454 | 40.412 | -2.371% | -3.166% |
| 1.19 fixture stock_sticker.html | 12.230 | 12.147 | +0.683% | +0.003% |
| 1.19 fixture trim_card.html | 49.316 | 48.911 | +0.828% | +0.008% |
| 1.19 100-row HTML render | 145.450 | 145.984 | -0.366% | +0.002% |
| 1.19 100-row uncompressed full-font render | 130.322 | 130.100 | +0.171% | -0.003% |
| 1.19 100-row PDF text extraction | 65.770 | 65.476 | +0.449% | +0.004% |
| 1.19 100-row PDF visual text extraction | 67.173 | 67.283 | -0.163% | -0.009% |
| 1.19 repeated text layout | 26.253 | 26.515 | -0.988% | +0.008% |
| 1.19 long paragraph layout | 50.183 | 50.234 | -0.102% | -0.008% |
| 1.19 five repeated PNG images | 13.491 | 13.474 | +0.126% | +0.000% |
| 1.19 ten repeated 512px PNG images | 327.902 | 328.783 | -0.268% | -0.015% |
| 1.19 800-word inline paragraph | 40.541 | 40.533 | +0.020% | -0.020% |
| 1.19 160 table headings | 54.135 | 53.796 | +0.630% | -0.004% |
| 1.19 4,000 boxes on one page | 15.055 | 15.023 | +0.213% | -0.002% |
| 1.19 100 ordinary Text-field fill | 357.751 | 395.187 | -9.473% | -0.930% |
| 1.19 100 ordinary Text-field flatten | 69.043 | 69.454 | -0.592% | +0.008% |
| 1.20 fixture invoice_012.html | 26.762 | 26.461 | +1.138% | -0.026% |
| 1.20 fixture statement_012.html | 25.660 | 25.780 | -0.465% | -0.155% |
| 1.20 fixture multi_page_report_012.html | 28.277 | 28.332 | -0.194% | +0.002% |
| 1.20 fixture purchase_order.html | 37.921 | 38.325 | -1.054% | -0.013% |
| 1.20 fixture material_requisition.html | 53.162 | 52.610 | +1.049% | +0.017% |
| 1.20 fixture government_application_form.html | 37.892 | 39.871 | -4.964% | -3.088% |
| 1.20 fixture stock_sticker.html | 11.307 | 11.362 | -0.484% | -0.034% |
| 1.20 fixture trim_card.html | 47.245 | 47.039 | +0.438% | +0.033% |
| 1.20 100-row HTML render | 132.011 | 132.320 | -0.234% | -0.003% |
| 1.20 100-row uncompressed full-font render | 119.357 | 119.217 | +0.117% | +0.003% |
| 1.20 100-row PDF text extraction | 63.779 | 64.870 | -1.682% | +0.004% |
| 1.20 100-row PDF visual text extraction | 65.231 | 65.525 | -0.449% | -0.001% |
| 1.20 repeated text layout | 25.933 | 25.919 | +0.054% | +0.001% |
| 1.20 long paragraph layout | 43.642 | 43.511 | +0.301% | +0.004% |
| 1.20 five repeated PNG images | 13.509 | 13.458 | +0.379% | -0.000% |
| 1.20 ten repeated 512px PNG images | 322.823 | 322.615 | +0.064% | +0.005% |
| 1.20 800-word inline paragraph | 39.915 | 40.264 | -0.867% | +0.015% |
| 1.20 160 table headings | 52.541 | 53.088 | -1.030% | -0.020% |
| 1.20 4,000 boxes on one page | 12.353 | 12.306 | +0.382% | -0.013% |
| 1.20 100 ordinary Text-field fill | 385.929 | 389.877 | -1.013% | -0.788% |
| 1.20 100 ordinary Text-field flatten | 66.118 | 66.373 | -0.384% | +0.008% |

Final 61-sample follow-ups:

| Runtime and workload | Candidate ms | B41 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 100 ordinary Text-field fill | 356.578 | 395.600 | -9.864% | -0.929% |
| 1.20 100 ordinary Text-field fill | 380.694 | 385.358 | -1.210% | -0.798% |
| 1.19 100 ordinary Text-field flatten | 68.390 | 69.080 | -0.999% | +0.005% |
| 1.20 100 ordinary Text-field flatten | 66.333 | 66.295 | +0.057% | +0.006% |
| 1.19 100-row uncompressed full-font render | 130.732 | 131.138 | -0.310% | +0.005% |
| 1.20 100-row uncompressed full-font render | 120.417 | 120.542 | -0.104% | +0.001% |
| 1.19 800-word inline paragraph | 40.486 | 40.482 | +0.010% | +0.022% |
| 1.20 800-word inline paragraph | 39.941 | 39.859 | +0.206% | +0.010% |
| 1.19 fixture invoice_012.html | 27.036 | 27.222 | -0.683% | +0.015% |
| 1.20 fixture invoice_012.html | 26.673 | 26.930 | -0.954% | -0.056% |
| 1.19 fixture stock_sticker.html | 12.141 | 12.168 | -0.222% | -0.016% |
| 1.20 fixture stock_sticker.html | 11.482 | 11.485 | -0.026% | +0.022% |
| 1.19 100-row HTML render | 145.092 | 145.132 | -0.028% | +0.005% |
| 1.20 100-row HTML render | 132.626 | 132.479 | +0.111% | +0.008% |

Feature cold timing has one exploratory sample per revision. Warmed samples alternate revisions in fresh processes with one untimed operation and GC before each sample. Whole-VM memory sampling uses three runs at a 10 ms interval and excludes native allocations and brief peaks. It includes shared loaded code and retained comparison PDFs; it is not an isolated renderer-process peak.

| Runtime | Controls | Depth | Mode | Pages | Objects | Bytes | Cold ms | Candidate warm ms | B41 warm ms | Candidate sampled VM MiB | B41 sampled VM MiB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1.19 | 1 | 1 | interactive | 1 | 22 | 155358 | 19.730 | 17.869 | 18.926 | 69.58 | 71.91 |
| 1.20 | 1 | 1 | interactive | 1 | 22 | 155358 | 19.487 | 17.468 | 18.711 | 68.62 | 70.54 |
| 1.19 | 1 | 1 | static | 1 | 11 | 78484 | 11.977 | 10.877 | 10.925 | 67.47 | 67.53 |
| 1.20 | 1 | 1 | static | 1 | 11 | 78484 | 12.625 | 10.583 | 10.661 | 65.44 | 66.28 |
| 1.19 | 100 | 1 | interactive | 4 | 523 | 279536 | 78.229 | 60.200 | 89.250 | 88.20 | 87.61 |
| 1.20 | 100 | 1 | interactive | 4 | 523 | 279536 | 73.511 | 52.555 | 86.590 | 85.85 | 86.56 |
| 1.19 | 100 | 1 | static | 4 | 116 | 104274 | 37.616 | 44.437 | 43.603 | 72.79 | 72.66 |
| 1.20 | 100 | 1 | static | 4 | 116 | 104274 | 31.526 | 34.902 | 34.341 | 68.92 | 68.98 |
| 1.19 | 1000 | 1 | interactive | 37 | 5089 | 1403823 | 624.642 | 657.980 | capacity failure | 166.30 | unavailable |
| 1.20 | 1000 | 1 | interactive | 37 | 5089 | 1403823 | 621.097 | 629.132 | capacity failure | 185.30 | unavailable |
| 1.19 | 1000 | 1 | static | 37 | 1082 | 309770 | 360.430 | 375.522 | 377.413 | 112.05 | 123.55 |
| 1.20 | 1000 | 1 | static | 37 | 1082 | 309770 | 319.027 | 347.582 | 345.733 | 121.43 | 116.70 |
| 1.19 | 1 | 1 | interactive | 1 | 22 | 155358 | 20.153 | 17.985 | 18.873 | 68.93 | 71.16 |
| 1.20 | 1 | 1 | interactive | 1 | 22 | 155358 | 20.650 | 17.379 | 18.773 | 68.72 | 70.56 |
| 1.19 | 1 | 10 | interactive | 1 | 22 | 155358 | 18.773 | 19.524 | 19.860 | 73.12 | 72.40 |
| 1.20 | 1 | 10 | interactive | 1 | 22 | 155358 | 19.194 | 18.829 | 19.056 | 66.01 | 71.58 |
| 1.19 | 1 | 50 | interactive | 1 | 22 | 155358 | 20.613 | 20.470 | 20.852 | 76.82 | 82.15 |
| 1.20 | 1 | 50 | interactive | 1 | 22 | 155358 | 19.356 | 20.090 | 20.206 | 71.20 | 71.49 |

All 17,267 original inventory IDs remain. Broad control records stay partial, and counts remain 2,254 implemented, 4,693 partial, 2,202 planned, 421 printable exclusions and 7,697 non-printing records. The full 1.1.0 plan and cumulative release-performance audit remain open.

## Batch 43: language attributes and selectors

HTML `lang` and literal `xml:lang` attributes are preserved on supported elements,
including empty, unknown and padded values. CSS `:lang()` resolves the nearest
language attribute; an empty attribute stops inheritance. A literal HTML
`xml:lang` remains attribute data and does not alter content language. Actual
XML namespace language takes precedence in the canonical validator. Full
foreign-content and shadow-slot integration remain dependencies.

Selectors accept identifiers, quoted strings, escapes and comma-separated
language ranges. Extended filtering retains singleton boundaries, distinguishes
untagged content from `und`, and supports wildcard subtags. BCP 47 preferred
mappings, extension ordering and extlang forms use the pinned IANA registry dated
2026-09-17, SHA-256 `755fad43283be7b41ebe3c89ad054b6eaf928f404f9c0edb74799e0eab74beb1`.
The registry facts are compiled constants. Rendering performs no registry reads
or network requests. Malformed identifiers retain their identity; alias rewriting
does not give them another language's meaning.

Content-Language pragmas retain the last successfully processed value in tree
order. Missing, empty and comma-containing content is ignored; the first ASCII
whitespace token supplies the default otherwise. Inert template content does not
set the document default. The current public renderer has no HTTP-header input.

The HTML and language validators own these semantics and all range preparation.
CSS attribute and language selectors share string/identifier escape decoding.
Language and negation functions share balanced CSS parsing. Selector-list commas
inside functions do not split the outer selector list. Language normalization,
metadata processing and matching use the existing CSS work budget. No limit or
performance threshold changes. Style computations give nodes fresh identities;
language results belong to that computation's cache. Raw document context is
released before layout and PDF writing, and nested style computations restore
their containing document context. Metadata is traversed only when a matching
language selector needs the default. Language-dependent compound selectors stop
before language processing when earlier positive requirements cannot match.
Ordinary selectors retain their existing evaluation path.

Existing tests cover attributes, inheritance, namespace distinctions, metadata
order, aliases, extlang forms, private-use and malformed identifiers, wildcard
and singleton boundaries, quoted punctuation, escaped newlines, invalid selectors,
shared budget exhaustion and cache isolation. A nonmatching compound selector
does not resolve a large unused language value, while a matching selector fails
with the shared actionable resource diagnostic when its budget is exhausted.

The browser fixture retains words and URI regions and compares inheritance,
overrides, empty and unknown values, escaped arguments, language lists and wildcard
subtags. Pinned Chromium ignores the list and wildcard rules in the direct
fixture. Its reference uses equivalent separate `:lang(fr)`/`:lang(de)` selectors
and `:lang(de-DE)` for the single German tag. Only the native test renderer replaces
those rules with the specification features. Text, attributes, links, fonts,
page geometry and thresholds remain unchanged. The direct comparison's 3.349%
changed pixels is preserved as the browser capability disagreement; the equivalent
fixture passes the original less-than-1% threshold.

The final prepared source passes 1,325 unit tests with 100% coverage. Both
supported runtimes preserve byte equality for all 21 ordinary outputs, including
complete PDFs and extracted text. Language scaling captures compare against independently authored
CSS classes and require identical complete PDFs, ordered labels and every URI
region. The feature input and explicit-class reference differ intentionally;
their timing difference measures language-feature cost rather than an unchanged
input regression.

Performance review found avoidable DOM retention in the initial render-wide
language cache. The cache is now scoped to style computation and only prepared
for language-dependent selectors. The parser propagates that dependency during
its existing selector pass, including negations. Language caches and raw document
context leave scope before layout and writing; nested style computations restore
their containing context. No added DOM scan, heap adjustment, threshold change,
baseline reset or content omission.

Longer unchanged-workload repeats found remaining trim-card and stock-sticker
increases after context scoping. Mixed-module profiles localized these to Style;
parser and validator variants stayed within measurement variation. An attempted
empty-list callback optimization reduced work counts but increased elapsed time,
and was rejected. The final revision keeps the original ordinary selector
execution and original RenderCache callback. Only language-dependent compound
selectors short-circuit language resolution after positive requirements fail.

The context-retaining, unconditional-context, conditional-context and rejected
empty-callback sources, loaded beams, primary captures and longer repeats remain
in separate ignored snapshots and capture folders. Call-count, garbage-collection
and mixed-module localization profiles are excluded from acceptance timings.
An early localization sidecar corrects its embedded CPU label from 4,5 to actual
0,1. Final ordinary and feature measurements use CPUs 4,5 and two schedulers.

The final 24 reports are consolidated in ignored
`.quality/performance/results/1.1.0-batch-43/reports.json`; original report bytes
and digests are retained separately. Twelve retain fixed fee6c4d, exact accepted
B42 and candidate in both orders on both runtimes. Two use 31 alternating fresh
process samples for all 21 frozen ordinary operations, after five parent warmups
and one untimed operation plus GC in each sample process. Ten separately measure
language documents at 1/100/1,000 groups and depths 1/10/50.
Additional final-root repeats use 61 samples and include the invoice timing
outlier found during the 31-sample review.

| Runtime and ordinary workload | Candidate ms | B42 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 26.897 | 26.975 | -0.289% | +0.016% |
| 1.19 fixture statement_012.html | 26.245 | 26.182 | +0.241% | +0.012% |
| 1.19 fixture multi_page_report_012.html | 28.241 | 28.035 | +0.735% | -0.006% |
| 1.19 fixture purchase_order.html | 39.671 | 39.557 | +0.288% | +0.022% |
| 1.19 fixture material_requisition.html | 53.437 | 54.588 | -2.109% | -0.033% |
| 1.19 fixture government_application_form.html | 39.790 | 39.517 | +0.691% | -0.020% |
| 1.19 fixture stock_sticker.html | 12.130 | 12.189 | -0.484% | +0.003% |
| 1.19 fixture trim_card.html | 48.988 | 48.945 | +0.088% | -0.006% |
| 1.19 100-row HTML render | 143.002 | 143.591 | -0.410% | -0.022% |
| 1.19 100-row uncompressed full-font render | 129.646 | 129.964 | -0.245% | +0.009% |
| 1.19 100-row PDF text extraction | 65.723 | 65.314 | +0.626% | -0.004% |
| 1.19 100-row PDF visual text extraction | 67.606 | 67.253 | +0.525% | -0.004% |
| 1.19 repeated text layout | 26.117 | 26.144 | -0.103% | +0.004% |
| 1.19 long paragraph layout | 49.862 | 50.165 | -0.604% | -0.002% |
| 1.19 five repeated PNG images | 13.537 | 13.525 | +0.089% | -0.002% |
| 1.19 ten repeated 512px PNG images | 323.197 | 324.984 | -0.550% | -0.008% |
| 1.19 800-word inline paragraph | 43.282 | 43.608 | -0.748% | -0.038% |
| 1.19 160 table headings | 53.415 | 53.719 | -0.566% | -0.008% |
| 1.19 4,000 boxes on one page | 14.495 | 14.390 | +0.730% | +0.025% |
| 1.19 100 ordinary Text-field fill | 348.054 | 348.788 | -0.210% | -0.003% |
| 1.19 100 ordinary Text-field flatten | 28.971 | 29.317 | -1.180% | -0.007% |
| 1.20 fixture invoice_012.html | 26.478 | 26.179 | +1.142% | +0.027% |
| 1.20 fixture statement_012.html | 25.436 | 25.581 | -0.567% | -0.007% |
| 1.20 fixture multi_page_report_012.html | 27.968 | 27.989 | -0.075% | +0.009% |
| 1.20 fixture purchase_order.html | 38.766 | 38.800 | -0.088% | +0.002% |
| 1.20 fixture material_requisition.html | 50.824 | 50.827 | -0.006% | -0.017% |
| 1.20 fixture government_application_form.html | 37.763 | 37.954 | -0.503% | -0.004% |
| 1.20 fixture stock_sticker.html | 11.449 | 11.502 | -0.461% | -0.002% |
| 1.20 fixture trim_card.html | 47.999 | 48.582 | -1.200% | -0.017% |
| 1.20 100-row HTML render | 133.877 | 133.728 | +0.111% | +0.004% |
| 1.20 100-row uncompressed full-font render | 119.382 | 119.716 | -0.279% | +0.003% |
| 1.20 100-row PDF text extraction | 64.408 | 63.924 | +0.757% | +0.002% |
| 1.20 100-row PDF visual text extraction | 65.525 | 65.464 | +0.093% | -0.000% |
| 1.20 repeated text layout | 26.016 | 26.017 | -0.004% | +0.001% |
| 1.20 long paragraph layout | 43.504 | 43.476 | +0.064% | +0.007% |
| 1.20 five repeated PNG images | 13.433 | 13.408 | +0.186% | +0.003% |
| 1.20 ten repeated 512px PNG images | 322.480 | 323.776 | -0.400% | +0.016% |
| 1.20 800-word inline paragraph | 39.996 | 39.897 | +0.248% | +0.006% |
| 1.20 160 table headings | 52.171 | 53.344 | -2.199% | +0.006% |
| 1.20 4,000 boxes on one page | 12.180 | 12.212 | -0.262% | +0.001% |
| 1.20 100 ordinary Text-field fill | 344.578 | 343.619 | +0.279% | +0.003% |
| 1.20 100 ordinary Text-field flatten | 25.934 | 25.865 | +0.267% | +0.000% |

Longer 61-sample repeats of the final root source retain complete PDF equality.

| Runtime and workload | Repeat | Candidate ms | B42 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- | --- |
| 1.19 100 ordinary Text-field flatten | 1 | 28.263 | 28.223 | +0.142% | -0.000% |
| 1.20 fixture invoice_012.html | 1 | 26.254 | 26.169 | +0.325% | +0.025% |
| 1.19 fixture purchase_order.html | 1 | 39.289 | 39.451 | -0.411% | +0.001% |
| 1.20 fixture stock_sticker.html | 1 | 11.486 | 11.521 | -0.304% | -0.004% |
| 1.19 fixture trim_card.html | 1 | 48.288 | 48.406 | -0.244% | +0.012% |
| 1.19 100 ordinary Text-field flatten | 2 | 28.395 | 28.335 | +0.212% | +0.000% |
| 1.20 fixture invoice_012.html | 2 | 26.784 | 26.637 | +0.552% | +0.027% |
| 1.19 fixture purchase_order.html | 2 | 39.223 | 39.170 | +0.135% | -0.013% |
| 1.20 fixture stock_sticker.html | 2 | 11.023 | 11.183 | -1.431% | +0.001% |
| 1.19 fixture trim_card.html | 2 | 48.606 | 48.878 | -0.556% | -0.008% |
| 1.19 100 ordinary Text-field flatten | 3 | 27.945 | 27.861 | +0.301% | -0.000% |
| 1.20 fixture invoice_012.html | 3 | 26.521 | 26.459 | +0.234% | +0.034% |
| 1.19 fixture purchase_order.html | 3 | 39.534 | 39.265 | +0.685% | -0.014% |
| 1.20 fixture stock_sticker.html | 3 | 11.411 | 11.344 | +0.591% | +0.014% |
| 1.19 fixture trim_card.html | 3 | 49.231 | 49.271 | -0.081% | -0.006% |

The final 15 longer repeats include purchase order, trim card, flattening, stock sticker and the invoice outlier. Positive invoice medians are +0.234% to +0.552%, rather than the original +1.142% result. Every positive paired estimate has a bootstrap 95% interval that includes zero. The review retains all higher samples and uses mean log-ratio resampling with seed 4305 and 10,000 resamples. No repeated unchanged-workload slowdown beyond measurement variation remains demonstrated for this batch. The fixed pre-milestone cumulative audit remains open.

Ordinary workload memory uses three additional whole-VM runs per revision and workload, sampled at 10 ms. These values include shared loaded code, parent-process state and retained PDFs; differences are not isolated renderer-process memory savings.

| Runtime and workload | Candidate maximum sampled VM MiB | B42 maximum sampled VM MiB | Output bytes |
| --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 78.60 | 144.95 | 162435 |
| 1.19 fixture statement_012.html | 78.34 | 78.35 | 161286 |
| 1.19 fixture multi_page_report_012.html | 76.71 | 78.40 | 162770 |
| 1.19 fixture purchase_order.html | 90.27 | 89.18 | 124909 |
| 1.19 fixture material_requisition.html | 95.34 | 96.79 | 126178 |
| 1.19 fixture government_application_form.html | 89.82 | 89.76 | 250479 |
| 1.19 fixture stock_sticker.html | 74.99 | 75.00 | 59392 |
| 1.19 fixture trim_card.html | 89.98 | 89.45 | 163615 |
| 1.19 100-row HTML render | 101.18 | 101.18 | 166773 |
| 1.19 100-row uncompressed full-font render | 82.11 | 85.29 | 1630334 |
| 1.19 100-row PDF text extraction | 85.51 | 85.52 | 9171 |
| 1.19 100-row PDF visual text extraction | 83.13 | 96.19 | 23286 |
| 1.19 repeated text layout | 77.17 | 77.18 | 82140 |
| 1.19 long paragraph layout | 83.17 | 90.94 | 85076 |
| 1.19 five repeated PNG images | 75.02 | 74.99 | 37811 |
| 1.19 ten repeated 512px PNG images | 89.77 | 87.72 | 634859 |
| 1.19 800-word inline paragraph | 86.66 | 84.64 | 84761 |
| 1.19 160 table headings | 93.79 | 81.98 | 155020 |
| 1.19 4,000 boxes on one page | 76.64 | 76.61 | 21226 |
| 1.19 100 ordinary Text-field fill | 266.05 | 265.99 | 308620 |
| 1.19 100 ordinary Text-field flatten | 74.56 | 74.55 | 316422 |
| 1.20 fixture invoice_012.html | 81.29 | 136.89 | 162435 |
| 1.20 fixture statement_012.html | 79.18 | 92.27 | 161286 |
| 1.20 fixture multi_page_report_012.html | 79.32 | 79.29 | 162770 |
| 1.20 fixture purchase_order.html | 92.38 | 92.40 | 124909 |
| 1.20 fixture material_requisition.html | 84.08 | 85.99 | 126178 |
| 1.20 fixture government_application_form.html | 81.19 | 81.15 | 250479 |
| 1.20 fixture stock_sticker.html | 77.68 | 77.68 | 59392 |
| 1.20 fixture trim_card.html | 92.31 | 92.72 | 163615 |
| 1.20 100-row HTML render | 91.74 | 93.44 | 166773 |
| 1.20 100-row uncompressed full-font render | 98.49 | 98.53 | 1630334 |
| 1.20 100-row PDF text extraction | 88.27 | 85.80 | 9171 |
| 1.20 100-row PDF visual text extraction | 85.74 | 85.67 | 23286 |
| 1.20 repeated text layout | 79.81 | 79.83 | 82140 |
| 1.20 long paragraph layout | 82.64 | 82.56 | 85076 |
| 1.20 five repeated PNG images | 77.68 | 77.65 | 37811 |
| 1.20 ten repeated 512px PNG images | 90.05 | 95.50 | 634859 |
| 1.20 800-word inline paragraph | 93.52 | 93.53 | 84761 |
| 1.20 160 table headings | 84.56 | 96.42 | 155020 |
| 1.20 4,000 boxes on one page | 79.78 | 79.75 | 21226 |
| 1.20 100 ordinary Text-field fill | 268.66 | 268.68 | 308620 |
| 1.20 100 ordinary Text-field flatten | 76.45 | 76.33 | 316422 |

| Runtime | Groups | Depth | Pages | Bytes | Exploratory cold ms | Candidate warm ms | Explicit-class reference warm ms | Candidate sampled VM MiB | Reference sampled VM MiB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1.19 | 1 | 1 | 1 | 78386 | 13.095 | 12.033 | 10.751 | 64.38 | 64.53 |
| 1.20 | 1 | 1 | 1 | 78386 | 12.407 | 11.944 | 10.695 | 65.58 | 64.01 |
| 1.19 | 100 | 1 | 2 | 100673 | 47.361 | 32.028 | 33.911 | 71.67 | 74.21 |
| 1.20 | 100 | 1 | 2 | 100673 | 38.737 | 36.169 | 32.652 | 72.43 | 80.71 |
| 1.19 | 100 | 10 | 2 | 100673 | 57.039 | 62.854 | 58.722 | 78.33 | 76.87 |
| 1.20 | 100 | 10 | 2 | 100673 | 65.121 | 60.851 | 57.879 | 86.07 | 85.75 |
| 1.19 | 100 | 50 | 2 | 100673 | 192.375 | 192.498 | 176.699 | 115.25 | 119.09 |
| 1.20 | 100 | 50 | 2 | 100673 | 194.427 | 183.171 | 181.475 | 116.61 | 116.54 |
| 1.19 | 1000 | 1 | 15 | 290082 | 346.905 | 348.510 | 327.286 | 112.22 | 112.11 |
| 1.20 | 1000 | 1 | 15 | 290082 | 336.824 | 301.865 | 291.247 | 108.09 | 117.79 |

Cold results are single exploratory first-feature calls in an already loaded VM,
not whole-process startup measurements. Warm samples use the same alternating
fresh-process policy as ordinary captures. Memory uses three whole-VM runs sampled
at 10 ms. It includes shared loaded code and retained PDFs, excludes native
allocations and brief peaks, and is not an isolated renderer-process peak.

Required final root validation uses the full supported-runtime matrix and exact
host browser command, retaining all 125 comparisons and their thresholds.
The batch acceptance record preserves the final log locations and outcomes.
All 17,267 original inventory IDs remain; broad language and metadata rows stay
partial until foreign-content, shadow/slot and other metadata dependencies are
complete. Categories remain 2,254 implemented, 4,693 partial, 2,202 planned,
421 printable exclusions and 7,697 non-printing records. The full 1.1.0 plan
and cumulative release-performance audit remain open.


## Batch 44: builtin PDF font resource reuse

PDF font collection now reuses an existing builtin resource without constructing unused entries or collecting text from later boxes. Initial entries, embedded text/glyph/spacing collection, text operators, validation, shared budgets and resource ordering retain their accepted behavior. No caller options or visible output change.

The earlier candidate also removed eager entry creation for embedded resources. It produced byte-identical output but repeatable inline timing increases of +1.608% to +2.774%, so it was rejected. The final change is confined to builtin reuse. Rejected source, compiled applications and every higher capture remain in ignored diagnostic folders.

An interleaved builtin/embedded writer regression lives in the existing writer test file. It covers explicit glyph IDs, spacing, font reuse, extraction, compression and subsetting. Complete-output comparisons retain every PDF byte and both extraction outputs across all 21 ordinary operations.

### Performance evidence

All 12 original guardrail captures use the fixed fee6c4d revision, accepted B43 and current source, in both capture orders on each supported runtime. Checks, fixtures, limits and thresholds are unchanged. Two alternating full-suite captures use 31 samples per revision, five warmups and one untimed operation plus garbage collection in each fresh sample process. Captures use the same two-scheduler containers and CPU affinity 4,5.

| Runtime and workload | Candidate ms | B43 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 26.947 | 26.919 | +0.104% | +0.041% |
| 1.19 fixture statement_012.html | 26.579 | 26.340 | +0.907% | +0.022% |
| 1.19 fixture multi_page_report_012.html | 28.571 | 28.622 | -0.178% | +0.036% |
| 1.19 fixture purchase_order.html | 39.461 | 39.180 | +0.717% | +0.009% |
| 1.19 fixture material_requisition.html | 52.591 | 52.279 | +0.597% | +0.034% |
| 1.19 fixture government_application_form.html | 39.513 | 39.504 | +0.023% | -0.007% |
| 1.19 fixture stock_sticker.html | 12.087 | 11.694 | +3.361% | +0.040% |
| 1.19 fixture trim_card.html | 49.097 | 49.206 | -0.222% | -0.012% |
| 1.19 100-row HTML render | 146.747 | 145.698 | +0.720% | +0.003% |
| 1.19 100-row uncompressed full-font render | 129.236 | 129.517 | -0.217% | -0.000% |
| 1.19 100-row PDF text extraction | 65.953 | 65.740 | +0.324% | -0.003% |
| 1.19 100-row PDF visual text extraction | 67.032 | 66.982 | +0.075% | +0.006% |
| 1.19 repeated text layout | 25.484 | 25.435 | +0.193% | +0.004% |
| 1.19 long paragraph layout | 49.665 | 49.894 | -0.459% | -0.001% |
| 1.19 five repeated PNG images | 13.522 | 13.541 | -0.140% | +0.002% |
| 1.19 ten repeated 512px PNG images | 329.372 | 328.199 | +0.357% | -0.007% |
| 1.19 800-word inline paragraph | 40.475 | 40.222 | +0.629% | +0.016% |
| 1.19 160 table headings | 52.313 | 52.881 | -1.074% | -0.075% |
| 1.19 4,000 boxes on one page | 14.445 | 14.918 | -3.171% | -4.277% |
| 1.19 100 ordinary Text-field fill | 349.707 | 349.603 | +0.030% | +0.000% |
| 1.19 100 ordinary Text-field flatten | 28.081 | 28.222 | -0.500% | -0.008% |
| 1.20 fixture invoice_012.html | 26.648 | 26.359 | +1.096% | +0.007% |
| 1.20 fixture statement_012.html | 25.739 | 25.660 | +0.308% | +0.012% |
| 1.20 fixture multi_page_report_012.html | 27.545 | 28.114 | -2.024% | -0.044% |
| 1.20 fixture purchase_order.html | 40.698 | 40.272 | +1.058% | +0.021% |
| 1.20 fixture material_requisition.html | 52.965 | 52.991 | -0.049% | -0.002% |
| 1.20 fixture government_application_form.html | 39.128 | 38.979 | +0.382% | +0.010% |
| 1.20 fixture stock_sticker.html | 11.514 | 11.499 | +0.130% | +0.004% |
| 1.20 fixture trim_card.html | 49.114 | 49.605 | -0.990% | -0.027% |
| 1.20 100-row HTML render | 134.424 | 134.260 | +0.122% | -0.003% |
| 1.20 100-row uncompressed full-font render | 121.285 | 121.291 | -0.005% | +0.003% |
| 1.20 100-row PDF text extraction | 64.179 | 63.824 | +0.556% | +0.009% |
| 1.20 100-row PDF visual text extraction | 65.490 | 65.481 | +0.014% | +0.006% |
| 1.20 repeated text layout | 26.049 | 26.068 | -0.073% | +0.007% |
| 1.20 long paragraph layout | 44.215 | 44.024 | +0.434% | +0.008% |
| 1.20 five repeated PNG images | 13.474 | 13.488 | -0.104% | +0.001% |
| 1.20 ten repeated 512px PNG images | 320.549 | 321.491 | -0.293% | -0.010% |
| 1.20 800-word inline paragraph | 40.746 | 40.654 | +0.226% | +0.009% |
| 1.20 160 table headings | 53.646 | 53.523 | +0.230% | +0.005% |
| 1.20 4,000 boxes on one page | 11.575 | 12.093 | -4.283% | -6.305% |
| 1.20 100 ordinary Text-field fill | 345.447 | 344.151 | +0.377% | -0.002% |
| 1.20 100 ordinary Text-field flatten | 25.872 | 25.787 | +0.330% | +0.000% |

Longer final-root repeats use 61 samples per revision and preserve all output bytes.

| Runtime and workload | Repeat | Candidate ms | B43 ms | Time change | Reduction change |
| --- | --- | --- | --- | --- | --- |
| 1.19 4,000 boxes on one page | 1 | 13.923 | 14.369 | -3.104% | -4.293% |
| 1.20 4,000 boxes on one page | 1 | 11.639 | 12.180 | -4.442% | -6.343% |
| 1.19 800-word inline paragraph | 1 | 40.172 | 40.281 | -0.271% | -0.016% |
| 1.19 fixture stock_sticker.html | 1 | 12.221 | 12.176 | +0.370% | +0.002% |
| 1.20 fixture invoice_012.html | 1 | 26.414 | 26.468 | -0.204% | -0.009% |
| 1.20 fixture purchase_order.html | 1 | 40.566 | 40.913 | -0.848% | -0.013% |
| 1.20 fixture statement_012.html | 1 | 25.579 | 25.756 | -0.687% | +0.008% |
| 1.19 4,000 boxes on one page | 2 | 14.047 | 14.652 | -4.129% | -4.283% |
| 1.20 4,000 boxes on one page | 2 | 11.616 | 12.113 | -4.103% | -6.288% |
| 1.19 800-word inline paragraph | 2 | 40.554 | 40.842 | -0.705% | -0.020% |
| 1.19 fixture stock_sticker.html | 2 | 12.130 | 12.168 | -0.312% | -0.000% |
| 1.20 fixture invoice_012.html | 2 | 26.706 | 26.449 | +0.972% | +0.031% |
| 1.20 fixture purchase_order.html | 2 | 40.656 | 40.810 | -0.377% | -0.019% |
| 1.20 fixture statement_012.html | 2 | 25.871 | 25.853 | +0.070% | +0.033% |
| 1.19 4,000 boxes on one page | 3 | 14.253 | 14.762 | -3.448% | -4.290% |
| 1.20 4,000 boxes on one page | 3 | 11.416 | 12.050 | -5.261% | -6.314% |
| 1.19 800-word inline paragraph | 3 | 40.291 | 40.283 | +0.020% | +0.006% |
| 1.19 fixture stock_sticker.html | 3 | 12.156 | 12.152 | +0.033% | +0.013% |
| 1.20 fixture invoice_012.html | 3 | 26.832 | 26.536 | +1.115% | +0.041% |
| 1.20 fixture purchase_order.html | 3 | 40.636 | 40.630 | +0.015% | +0.015% |
| 1.20 fixture statement_012.html | 3 | 25.842 | 25.853 | -0.043% | +0.012% |
| 1.20 fixture invoice_012.html | 4 | 26.466 | 26.376 | +0.341% | +0.025% |
| 1.20 fixture invoice_012.html | 5 | 26.249 | 26.439 | -0.719% | -0.016% |
| 1.20 fixture invoice_012.html | 6 | 26.224 | 26.256 | -0.122% | +0.005% |

Statistical review keeps every higher sample and pairs original iterations. Per-capture mean log-ratio intervals use seed 4305 and 10,000 resamples. Two invoice captures retain positive intervals and +0.972%/+1.115% medians. Three additional unchanged-harness repeats are +0.341%, -0.719% and -0.122%. Across all six independent invoice captures, the paired log-mean estimate is +0.141%, with a hierarchical 95% interval from -0.389% to +0.688%. This review resamples captures and then original paired samples, using seed 4405 and 10,000 resamples. Every ordinary group includes variation across at least three captures; no positive group interval is wholly above zero.

Excluded fixed-revision VM invoice comparisons are -0.121%, -0.873% and -0.625%, with complete outputs and inputs preserved. Call-count and GC profiling gives identical invoice counts. A separate 20-ms pause outside the measured operation produces +0.883%, -0.695% and -0.184%. These setup checks support a benchmark-interference interpretation, but do not prove its exact mechanism or replace the higher primary captures. The complete set does not demonstrate a stable invoice slowdown. The broader fixed-baseline release audit remains open.

| Runtime and original writer capture | Candidate ms | Fixed baseline ms | Time change | Reduction change |
| --- | --- | --- | --- | --- |
| 1.19 order 1 | 12.948 | 13.072 | -0.949% | -2.702% |
| 1.19 order 2 | 13.455 | 13.254 | +1.517% | -2.706% |
| 1.20 order 1 | 10.656 | 10.801 | -1.342% | -3.639% |
| 1.20 order 2 | 10.563 | 10.534 | +0.275% | -3.647% |

Excluded call-count profiling uses three exact original 4,000-box operations per revision. It records source, beam and input hashes and compares the complete 21,226-byte PDFs. The accepted B43 performs 12,000 font-entry constructions and 23,997 text-entry preparations; the final builtin-only writer performs three of each. Writer validation calls remain unchanged. Profiling is separate from acceptance timings.

Existing font paths are measured separately at 1, 100 and 1,000 runs. All 24 primary comparisons preserve identical complete PDFs, font bytes and inputs.

| Runtime | Font path | Runs | Bytes | Candidate warm ms | B43 warm ms | Candidate exploratory cold ms | B43 exploratory cold ms | Candidate sampled VM MiB | B43 sampled VM MiB |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1.19 | builtin | 1 | 604 | 0.019 | 0.019 | 0.033 | 4.351 | 54.56 | 54.59 |
| 1.19 | builtin | 100 | 1033 | 0.305 | 0.319 | 0.604 | 4.686 | 54.02 | 54.04 |
| 1.19 | builtin | 1000 | 3432 | 2.643 | 2.788 | 2.969 | 7.372 | 56.67 | 56.66 |
| 1.19 | embedded | 1 | 77071 | 6.464 | 6.485 | 6.961 | 12.842 | 58.34 | 58.32 |
| 1.19 | embedded | 100 | 79461 | 12.393 | 12.388 | 7.589 | 14.657 | 114.69 | 114.71 |
| 1.19 | embedded | 1000 | 82309 | 103.286 | 103.191 | 15.185 | 24.517 | 567.29 | 567.32 |
| 1.19 | glyph | 1 | 76646 | 7.477 | 7.469 | 10.081 | 15.944 | 57.56 | 57.55 |
| 1.19 | glyph | 100 | 78966 | 23.947 | 24.375 | 9.470 | 15.814 | 115.10 | 115.11 |
| 1.19 | glyph | 1000 | 81809 | 98.706 | 97.755 | 15.619 | 22.371 | 566.13 | 566.13 |
| 1.19 | mixed | 1 | 604 | 0.019 | 0.019 | 0.033 | 4.516 | 56.02 | 55.99 |
| 1.19 | mixed | 100 | 80120 | 10.493 | 10.536 | 9.653 | 16.562 | 82.82 | 82.85 |
| 1.19 | mixed | 1000 | 82973 | 48.472 | 48.456 | 17.619 | 23.311 | 301.15 | 301.73 |
| 1.20 | builtin | 1 | 604 | 0.020 | 0.021 | 0.028 | 3.597 | 57.60 | 57.74 |
| 1.20 | builtin | 100 | 1033 | 0.301 | 0.313 | 0.836 | 3.890 | 57.55 | 57.58 |
| 1.20 | builtin | 1000 | 3432 | 2.539 | 2.657 | 2.966 | 6.390 | 58.00 | 58.00 |
| 1.20 | embedded | 1 | 77071 | 6.476 | 6.553 | 6.252 | 13.470 | 59.65 | 59.65 |
| 1.20 | embedded | 100 | 79461 | 12.745 | 12.762 | 7.577 | 13.391 | 115.80 | 115.81 |
| 1.20 | embedded | 1000 | 82309 | 105.390 | 105.187 | 14.140 | 21.904 | 567.62 | 567.62 |
| 1.20 | glyph | 1 | 76646 | 7.400 | 7.370 | 9.904 | 14.432 | 61.13 | 61.13 |
| 1.20 | glyph | 100 | 78966 | 24.167 | 24.374 | 9.002 | 14.885 | 116.45 | 116.41 |
| 1.20 | glyph | 1000 | 81809 | 90.437 | 90.552 | 16.487 | 21.914 | 568.18 | 568.21 |
| 1.20 | mixed | 1 | 604 | 0.022 | 0.021 | 0.046 | 3.563 | 57.63 | 57.61 |
| 1.20 | mixed | 100 | 80120 | 10.776 | 10.776 | 8.615 | 15.703 | 85.88 | 87.13 |
| 1.20 | mixed | 1000 | 82973 | 50.947 | 51.324 | 16.680 | 22.065 | 305.60 | 305.45 |

Cold calls are single exploratory writer calls in an already loaded VM, with fonts prepared. They do not measure process startup. Each revision has three additional whole-VM memory runs sampled at 10 ms. These values include loaded code/fonts and retained inputs/outputs, exclude native allocations and brief peaks, and do not isolate renderer-process savings. Longer font repeats retain each primary timing outlier for review.

| Runtime and workload | Candidate maximum sampled VM MiB | B43 maximum sampled VM MiB | Output bytes |
| --- | --- | --- | --- |
| 1.19 fixture invoice_012.html | 87.38 | 139.93 | 162438 |
| 1.19 fixture statement_012.html | 87.96 | 75.04 | 161286 |
| 1.19 fixture multi_page_report_012.html | 75.14 | 75.14 | 162773 |
| 1.19 fixture purchase_order.html | 94.75 | 88.67 | 124908 |
| 1.19 fixture material_requisition.html | 88.15 | 94.74 | 126176 |
| 1.19 fixture government_application_form.html | 80.26 | 80.07 | 250478 |
| 1.19 fixture stock_sticker.html | 73.34 | 73.35 | 59392 |
| 1.19 fixture trim_card.html | 83.20 | 87.84 | 163622 |
| 1.19 100-row HTML render | 100.41 | 89.15 | 166773 |
| 1.19 100-row uncompressed full-font render | 88.33 | 82.97 | 1630334 |
| 1.19 100-row PDF text extraction | 83.95 | 83.89 | 9171 |
| 1.19 100-row PDF visual text extraction | 81.41 | 81.71 | 23286 |
| 1.19 repeated text layout | 75.49 | 75.48 | 82140 |
| 1.19 long paragraph layout | 89.33 | 79.15 | 85076 |
| 1.19 five repeated PNG images | 73.35 | 73.38 | 37811 |
| 1.19 ten repeated 512px PNG images | 83.91 | 91.50 | 634859 |
| 1.19 800-word inline paragraph | 89.17 | 82.98 | 84761 |
| 1.19 160 table headings | 92.14 | 80.39 | 155020 |
| 1.19 4,000 boxes on one page | 75.02 | 74.99 | 21226 |
| 1.19 100 ordinary Text-field fill | 264.43 | 264.39 | 308620 |
| 1.19 100 ordinary Text-field flatten | 72.94 | 72.91 | 316422 |
| 1.20 fixture invoice_012.html | 79.02 | 142.56 | 162438 |
| 1.20 fixture statement_012.html | 90.60 | 90.60 | 161286 |
| 1.20 fixture multi_page_report_012.html | 79.70 | 77.68 | 162773 |
| 1.20 fixture purchase_order.html | 97.35 | 90.00 | 124908 |
| 1.20 fixture material_requisition.html | 91.88 | 97.29 | 126176 |
| 1.20 fixture government_application_form.html | 82.81 | 82.88 | 250478 |
| 1.20 fixture stock_sticker.html | 76.02 | 76.03 | 59392 |
| 1.20 fixture trim_card.html | 89.94 | 90.46 | 163622 |
| 1.20 100-row HTML render | 90.13 | 93.74 | 166773 |
| 1.20 100-row uncompressed full-font render | 85.63 | 85.65 | 1630334 |
| 1.20 100-row PDF text extraction | 86.56 | 84.11 | 9171 |
| 1.20 100-row PDF visual text extraction | 86.56 | 86.59 | 23286 |
| 1.20 repeated text layout | 78.10 | 78.08 | 82140 |
| 1.20 long paragraph layout | 80.91 | 80.90 | 85076 |
| 1.20 five repeated PNG images | 75.99 | 76.01 | 37811 |
| 1.20 ten repeated 512px PNG images | 92.55 | 90.04 | 634859 |
| 1.20 800-word inline paragraph | 91.85 | 91.90 | 84761 |
| 1.20 160 table headings | 94.72 | 94.75 | 155020 |
| 1.20 4,000 boxes on one page | 78.05 | 78.09 | 21226 |
| 1.20 100 ordinary Text-field fill | 266.98 | 266.96 | 308620 |
| 1.20 100 ordinary Text-field flatten | 74.71 | 74.70 | 316422 |

All 38 primary reports are consolidated under ignored `.quality/performance/results/1.1.0-batch-44/reports.json`; original bytes and hashes remain under `.quality/performance/new-original-captures/`. Diagnostic variants, longer repeats and memory reports are also local. No benchmark JSON is added to Git.

Required final root validation uses the full supported-runtime matrix and the exact host browser command, retaining all 125 comparisons and their thresholds. The acceptance record preserves their final log paths and results.

This optimization does not change specification inventory categories. All 17,267 IDs remain: 2,254 implemented, 4,693 partial, 2,202 planned, 421 printable exclusions and 7,697 non-printing records. Full control state, foreign/embedded content, SVG/MathML dependencies and the cumulative release-performance audit remain unfinished. The full 1.1.0 plan is not complete.


## Batch 45: form ownership and initial radio state
Ordinary HTML forms render as blocks and labels render inline. Supported controls
resolve their nearest ancestor owner or an explicit form reference. Explicit
references use the first matching ID and require an HTML form; unresolved and
non-form targets do not fall back to an ancestor. Labels resolve explicit first-ID
targets or their first supported labelable descendant. Hidden inputs are excluded.
Nested forms and nested labels return source diagnostics. Harmless submission
metadata is retained without executing actions.

Initial radios group by actual owner and nonempty authored name within the
ordinary HTML tree. The last authored checked member wins. Empty and absent
names remain independent. Authored attributes remain intact for CSS attribute
selectors, while artwork and PDF values consume one canonical state. Independent
owner groups sharing a PDF field name return `:unsupported_form` with the field
name and an actionable unique-name or static-mode alternative. The low-level
writer diagnoses missing or malformed initial control state instead of raising.

The existing DOM validation reports whether active controls, forms or labels
need preparation. Dormant template contents do not set that flag. Documents
without those elements use the shared selector/inert/heading metadata traversal
and receive no additional feature traversal. Documents using controls collect
ownership and label facts after metadata assignment, then attach canonical state.
Implicit label targets are collected during traversal rather than searched
against every control. The fixture exposed a grid/flex child margin-collapse gap; normal block
parents now collapse with those children's outer margins. Existing and new layout
tests cover positive parent/child margins and the next sibling's position.

Broader label content rules, fieldsets and legends, selection defaults and
pseudo-classes, optgroups, constraints, additional input types and shadow/foreign
integration remain unfinished. This is a control-state foundation, not completion
of the broad controls milestone.

### Ordinary workload measurements

Both supported runtime images use two BEAM schedulers and CPU affinity 4,5.
The original 19 operations and two ordinary form fill/flatten operations retain
B44 inputs, options and assets. Every complete output is byte-identical to B44.
There are 31 alternating samples per revision, five warmups, and a fresh worker
after one untimed operation and GC for each sample. Module loads occur outside
measurement. The fixed fee6c4d captures remain preserved in the B44 bundle;
these are adjacent B44/B45 comparisons and do not close the cumulative audit.

| Runtime / workload | B44 ms | B45 ms | Time change | Reduction change |
| --- | ---: | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 26.742 | 26.777 | +0.131% | +0.014% |
| 1.19 fixture statement_012.html | 24.882 | 24.472 | -1.648% | +0.012% |
| 1.19 fixture multi_page_report_012.html | 28.525 | 28.104 | -1.476% | -0.017% |
| 1.19 fixture purchase_order.html | 39.350 | 42.000 | +6.734% | +0.257% |
| 1.19 fixture material_requisition.html | 52.581 | 52.920 | +0.645% | +0.047% |
| 1.19 fixture government_application_form.html | 38.861 | 39.060 | +0.512% | +0.056% |
| 1.19 fixture stock_sticker.html | 12.141 | 12.047 | -0.774% | +0.004% |
| 1.19 fixture trim_card.html | 49.602 | 50.284 | +1.375% | -0.018% |
| 1.19 100-row HTML render | 144.003 | 143.965 | -0.026% | +0.022% |
| 1.19 100-row uncompressed full-font render | 129.480 | 128.733 | -0.577% | +0.042% |
| 1.19 100-row PDF text extraction | 66.975 | 66.640 | -0.500% | -0.014% |
| 1.19 100-row PDF visual text extraction | 68.043 | 68.302 | +0.381% | +0.006% |
| 1.19 repeated text layout | 25.889 | 25.959 | +0.270% | +0.014% |
| 1.19 long paragraph layout | 50.320 | 50.414 | +0.187% | +0.003% |
| 1.19 five repeated PNG images | 13.518 | 13.462 | -0.414% | +0.022% |
| 1.19 ten repeated 512px PNG images | 324.908 | 326.546 | +0.504% | +0.004% |
| 1.19 800-word inline paragraph | 43.613 | 43.406 | -0.475% | +0.004% |
| 1.19 160 table headings | 53.304 | 53.546 | +0.454% | +0.095% |
| 1.19 4,000 boxes on one page | 14.520 | 14.420 | -0.689% | +0.017% |
| 1.19 100 ordinary Text-field fill | 345.395 | 345.902 | +0.147% | +0.003% |
| 1.19 100 ordinary Text-field flatten | 28.764 | 29.683 | +3.195% | +0.001% |
| 1.20 fixture invoice_012.html | 26.732 | 26.867 | +0.505% | +0.051% |
| 1.20 fixture statement_012.html | 25.746 | 25.936 | +0.738% | +0.034% |
| 1.20 fixture multi_page_report_012.html | 27.773 | 28.135 | +1.303% | +0.035% |
| 1.20 fixture purchase_order.html | 39.236 | 38.926 | -0.790% | +0.002% |
| 1.20 fixture material_requisition.html | 51.116 | 50.920 | -0.383% | +0.004% |
| 1.20 fixture government_application_form.html | 38.236 | 38.037 | -0.520% | +0.028% |
| 1.20 fixture stock_sticker.html | 11.432 | 11.464 | +0.280% | +0.058% |
| 1.20 fixture trim_card.html | 48.506 | 48.925 | +0.864% | +0.031% |
| 1.20 100-row HTML render | 133.188 | 132.861 | -0.246% | +0.006% |
| 1.20 100-row uncompressed full-font render | 120.529 | 121.002 | +0.392% | -0.002% |
| 1.20 100-row PDF text extraction | 64.326 | 64.020 | -0.476% | -0.009% |
| 1.20 100-row PDF visual text extraction | 66.054 | 66.063 | +0.014% | -0.015% |
| 1.20 repeated text layout | 26.226 | 26.285 | +0.225% | +0.002% |
| 1.20 long paragraph layout | 43.843 | 43.565 | -0.634% | -0.000% |
| 1.20 five repeated PNG images | 13.447 | 13.443 | -0.030% | +0.018% |
| 1.20 ten repeated 512px PNG images | 323.981 | 324.780 | +0.247% | -0.013% |
| 1.20 800-word inline paragraph | 40.436 | 40.273 | -0.403% | +0.014% |
| 1.20 160 table headings | 53.185 | 53.691 | +0.951% | +0.013% |
| 1.20 4,000 boxes on one page | 11.670 | 11.682 | +0.103% | +0.003% |
| 1.20 100 ordinary Text-field fill | 343.880 | 343.787 | -0.027% | -0.004% |
| 1.20 100 ordinary Text-field flatten | 25.916 | 26.155 | +0.922% | -0.000% |

The paired bootstrap review uses mean log time ratios, 10,000 resamples and
seed 4505. All original samples and review intervals are retained locally.
The positive Elixir 1.20 multi-page-report interval received three longer
61-sample repeats: +0.252%, -0.196% and -0.596%. These do not reproduce a
consistent increase. Elixir 1.19 purchase-order module-swapping repeats remain
+5.773%, +5.969% and +5.398%, rather than being discarded as noise.

Three additional separate-VM comparisons keep each revision loaded throughout
measurement, reverse VM order in the middle comparison, use 61 fresh-worker
samples and retain identical input and complete output hashes. Their B44/B45
medians are 41.994/39.548 ms, 42.427/39.938 ms and 41.742/39.875 ms:
-5.825%, -5.867% and -4.473%. The module-swapping slowdown is not reproduced
with a fixed revision per VM. The cause of that measurement discrepancy is
unproven; these results do not establish a release-wide speedup.

Native call-count profiling of three exact purchase-order operations preserves
every Layout function count. The metadata traversal has 366 recursive calls and
819 node callbacks in both revisions, moved from Style into HtmlValidator.
No control-state collector or attachment traversal runs for this ordinary input.
This profiling is excluded from timing claims. The rejected combined,
conditional-registration and scoped-context variants, their positive timings,
profiles and original report bytes are preserved locally. The selected path
avoids feature preparation for ordinary documents and leaves the cumulative
fixed-baseline release audit open.

### Feature scaling and memory

Feature workloads use implicit labels or forward explicit owners, with two radios
per group and both authored checked. Each feature PDF must exactly equal the B44
PDF for equivalent div/span markup with effective checkedness authored explicitly.
Content, all editable field values and page count are checked separately. Each
case has three warmups and seven fresh-worker timing samples. A first operation
per case is also retained; it is not a cold application-startup measurement.
Three separate whole-VM memory runs per case sample at 10 ms, include loaded
code and retained inputs/output, and exclude native allocations and brief peaks.
They are not isolated renderer-process memory measurements.

| Runtime / family | Groups / controls | Mode | Median ms | Reductions | Pages | Largest sampled VM MiB | PDF bytes |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 1.19 implicit_labels | 1 / 2 | static | 9.836 | 976818 | 1 | 64.67 | 77642 |
| 1.19 implicit_labels | 1 / 2 | interactive | 15.949 | 1411927 | 1 | 70.12 | 154961 |
| 1.19 implicit_labels | 100 / 200 | static | 33.979 | 3322824 | 7 | 78.26 | 83791 |
| 1.19 implicit_labels | 100 / 200 | interactive | 66.838 | 6549766 | 7 | 85.37 | 300819 |
| 1.19 implicit_labels | 1000 / 2000 | static | 371.270 | 25281619 | 68 | 128.34 | 125685 |
| 1.19 implicit_labels | 1000 / 2000 | interactive | 845.041 | 76057866 | 68 | 232.01 | 1654559 |
| 1.19 forward_owners | 1 / 2 | static | 9.017 | 954826 | 1 | 64.34 | 77642 |
| 1.19 forward_owners | 1 / 2 | interactive | 15.408 | 1394073 | 1 | 67.52 | 154961 |
| 1.19 forward_owners | 100 / 200 | static | 34.406 | 3417674 | 7 | 78.10 | 83795 |
| 1.19 forward_owners | 100 / 200 | interactive | 70.523 | 6643018 | 7 | 91.82 | 300816 |
| 1.19 forward_owners | 1000 / 2000 | static | 382.039 | 26306284 | 68 | 128.66 | 125726 |
| 1.19 forward_owners | 1000 / 2000 | interactive | 830.889 | 77006796 | 68 | 224.97 | 1654541 |
| 1.20 implicit_labels | 1 / 2 | static | 9.449 | 975946 | 1 | 65.09 | 77642 |
| 1.20 implicit_labels | 1 / 2 | interactive | 15.876 | 1396898 | 1 | 68.94 | 154961 |
| 1.20 implicit_labels | 100 / 200 | static | 33.647 | 3218158 | 7 | 73.52 | 83791 |
| 1.20 implicit_labels | 100 / 200 | interactive | 67.742 | 6135586 | 7 | 85.94 | 300819 |
| 1.20 implicit_labels | 1000 / 2000 | static | 354.907 | 24173890 | 68 | 140.13 | 125685 |
| 1.20 implicit_labels | 1000 / 2000 | interactive | 810.637 | 71946562 | 68 | 211.74 | 1654559 |
| 1.20 forward_owners | 1 / 2 | static | 9.101 | 977063 | 1 | 64.95 | 77642 |
| 1.20 forward_owners | 1 / 2 | interactive | 15.296 | 1384089 | 1 | 70.87 | 154961 |
| 1.20 forward_owners | 100 / 200 | static | 34.262 | 3333558 | 7 | 81.58 | 83795 |
| 1.20 forward_owners | 100 / 200 | interactive | 67.199 | 6239991 | 7 | 87.10 | 300816 |
| 1.20 forward_owners | 1000 / 2000 | static | 405.821 | 25437855 | 68 | 131.66 | 125726 |
| 1.20 forward_owners | 1000 / 2000 | interactive | 845.351 | 73165019 | 68 | 225.00 | 1654541 |

Ordinary memory checks retain all 21 exact operations and compare complete output
hashes across three runs per revision. The same sampling limitations apply.

| Runtime / workload | Largest B44 VM MiB | Largest B45 VM MiB | Output bytes |
| --- | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 142.60 | 77.87 | 162438 |
| 1.19 fixture statement_012.html | 86.95 | 82.10 | 161286 |
| 1.19 fixture multi_page_report_012.html | 78.01 | 77.99 | 162773 |
| 1.19 fixture purchase_order.html | 84.35 | 90.24 | 124908 |
| 1.19 fixture material_requisition.html | 97.49 | 97.48 | 126176 |
| 1.19 fixture government_application_form.html | 83.09 | 78.00 | 250478 |
| 1.19 fixture stock_sticker.html | 76.27 | 76.30 | 59392 |
| 1.19 fixture trim_card.html | 91.38 | 91.23 | 163622 |
| 1.19 100-row HTML render | 92.06 | 103.31 | 166773 |
| 1.19 100-row uncompressed full-font render | 85.97 | 85.76 | 1630334 |
| 1.19 100-row PDF text extraction | 97.48 | 84.62 | 9171 |
| 1.19 100-row PDF visual text extraction | 84.44 | 86.85 | 23286 |
| 1.19 repeated text layout | 78.46 | 78.46 | 82140 |
| 1.19 long paragraph layout | 91.77 | 92.27 | 85076 |
| 1.19 five repeated PNG images | 76.29 | 76.29 | 37811 |
| 1.19 ten repeated 512px PNG images | 92.80 | 88.97 | 634859 |
| 1.19 800-word inline paragraph | 92.23 | 92.54 | 84761 |
| 1.19 160 table headings | 83.65 | 95.10 | 155020 |
| 1.19 4,000 boxes on one page | 77.98 | 77.97 | 21226 |
| 1.19 100 ordinary Text-field fill | 267.33 | 267.30 | 308620 |
| 1.19 100 ordinary Text-field flatten | 75.84 | 75.88 | 316422 |
| 1.20 fixture invoice_012.html | 157.92 | 81.63 | 162438 |
| 1.20 fixture statement_012.html | 81.40 | 81.36 | 161286 |
| 1.20 fixture multi_page_report_012.html | 79.63 | 81.68 | 162773 |
| 1.20 fixture purchase_order.html | 99.31 | 99.31 | 124908 |
| 1.20 fixture material_requisition.html | 99.08 | 99.08 | 126176 |
| 1.20 fixture government_application_form.html | 92.85 | 84.79 | 250478 |
| 1.20 fixture stock_sticker.html | 74.66 | 78.01 | 59392 |
| 1.20 fixture trim_card.html | 92.48 | 92.48 | 163622 |
| 1.20 100-row HTML render | 104.75 | 92.11 | 166773 |
| 1.20 100-row uncompressed full-font render | 87.72 | 87.20 | 1630334 |
| 1.20 100-row PDF text extraction | 86.14 | 86.14 | 9171 |
| 1.20 100-row PDF visual text extraction | 88.56 | 86.15 | 23286 |
| 1.20 repeated text layout | 80.08 | 80.10 | 82140 |
| 1.20 long paragraph layout | 82.96 | 82.94 | 85076 |
| 1.20 five repeated PNG images | 78.02 | 78.00 | 37811 |
| 1.20 ten repeated 512px PNG images | 94.56 | 91.23 | 634859 |
| 1.20 800-word inline paragraph | 89.78 | 93.92 | 84761 |
| 1.20 160 table headings | 85.35 | 96.72 | 155020 |
| 1.20 4,000 boxes on one page | 80.12 | 79.75 | 21226 |
| 1.20 100 ordinary Text-field fill | 269.03 | 269.00 | 308620 |
| 1.20 100 ordinary Text-field flatten | 76.69 | 76.71 | 316422 |

### Validation and remaining work

The focused static fixture preserves all captions and one page, with 0.592%
changed pixels against scripting-disabled Chromium under the unchanged 1% limit.
Native circle artwork differs from Chromium's control skin. Existing test files
cover ownership, first-ID behavior, forward/unresolved references, separate
owners, nameless groups, template isolation, label targets, raw checked selectors,
interactive values, static output, margin geometry and public diagnostics.
The full quality matrix passes both supported runtimes with 1,335 unit tests,
100% coverage and all 126 browser comparisons per runtime. There are no WARN
or FAIL entries. Logs: `.quality/logs/20261011T020318Z-863014`. The exact host browser
command also passes all 126 comparisons.

Original inventory IDs are retained. Form, label and initial owner-reset rows move
from planned to partial, while radio groups remain partial. The complete inventory
still has 17,267 records: 2,254 implemented, 4,696 partial, 2,199 planned,
421 printable exclusions and 7,697 non-printing records. Classification accounting
is complete; implementation coverage is not. Full 1.1.0 implementation and the
cumulative release-performance audit remain open.

All benchmark reports are ignored under `.quality/performance/`, with consolidated
bundles and original report bytes/digests preserved. No benchmark JSON is tracked.


## Batch 46: dropdown selection and checked selectors
Ordinary single-select dropdowns compute initial selectedness once in
HtmlFormValidator. The last authored selected option wins, even when disabled.
Without an authored selection a normal dropdown selects its first enabled option.
Empty and all-disabled unselected dropdowns have no selection; an empty option
can be selected and retains the empty string value. Authored attributes remain
intact for CSS attribute selectors. Static artwork and editable PDF values consume
canonical state. PDF choice serialization omits V when no selection exists.

Initial :checked matching applies to effective ordinary checkbox/radio checkedness
and option selectedness. CSS escapes, ASCII keyword case and negation use the
existing CSS parser and work budget. Option states are attached during the
existing control-state traversal. Dropdown choice/value preparation is removed
from downstream Style and PDF-control assembly, rather than kept as a second
interpretation of selected attributes. Malformed low-level selection state returns
an actionable forms diagnostic instead of reaching PDF serialization.

The B45 feature-detection flag is unchanged. Ordinary documents receive no new
feature traversal. Control state and selected option IDs are collected together;
input-only documents do not receive a separate selection scan. Six focused tests
extend existing files. Broader dropdown sizing, multiple selections, optgroups,
option label/text/value semantics, constraints, richer controls and tree integration
remain unfinished. The full controls milestone and the full 1.1.0 plan remain open.

### Ordinary workload measurements

Both supported runtime images use two BEAM schedulers and CPU affinity 4,5.
The original 19 operations and two ordinary form fill/flatten operations retain
B45 inputs, options and assets. Every complete output is byte-identical to B45.
There are 31 alternating samples per revision, five warmups, and a fresh worker
after one untimed operation and GC for each sample. Module loads occur outside
measurement. The fixed fee6c4d captures remain preserved in the B44 bundle;
these are adjacent B45/B46 comparisons and do not close the cumulative audit.

| Runtime / workload | B45 ms | B46 ms | Time change | Reduction change |
| --- | ---: | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 27.310 | 26.995 | -1.153% | -0.031% |
| 1.19 fixture statement_012.html | 26.452 | 26.227 | -0.851% | +0.003% |
| 1.19 fixture multi_page_report_012.html | 28.417 | 28.877 | +1.619% | -0.001% |
| 1.19 fixture purchase_order.html | 41.653 | 41.710 | +0.137% | +0.011% |
| 1.19 fixture material_requisition.html | 53.314 | 53.373 | +0.111% | +0.008% |
| 1.19 fixture government_application_form.html | 39.123 | 39.027 | -0.245% | +0.043% |
| 1.19 fixture stock_sticker.html | 12.133 | 12.189 | +0.462% | +0.015% |
| 1.19 fixture trim_card.html | 48.831 | 49.013 | +0.373% | -0.017% |
| 1.19 100-row HTML render | 144.968 | 145.310 | +0.236% | -0.011% |
| 1.19 100-row uncompressed full-font render | 129.654 | 129.932 | +0.214% | +0.003% |
| 1.19 100-row PDF text extraction | 65.930 | 65.663 | -0.405% | -0.006% |
| 1.19 100-row PDF visual text extraction | 67.896 | 67.797 | -0.146% | +0.002% |
| 1.19 repeated text layout | 25.821 | 25.867 | +0.178% | +0.003% |
| 1.19 long paragraph layout | 50.334 | 50.250 | -0.167% | -0.008% |
| 1.19 five repeated PNG images | 13.550 | 13.584 | +0.251% | +0.001% |
| 1.19 ten repeated 512px PNG images | 326.368 | 327.125 | +0.232% | +0.001% |
| 1.19 800-word inline paragraph | 43.937 | 43.697 | -0.546% | -0.012% |
| 1.19 160 table headings | 54.626 | 53.790 | -1.530% | -0.007% |
| 1.19 4,000 boxes on one page | 14.222 | 14.300 | +0.548% | -0.006% |
| 1.19 100 ordinary Text-field fill | 348.401 | 347.967 | -0.125% | +0.001% |
| 1.19 100 ordinary Text-field flatten | 30.264 | 30.253 | -0.036% | +0.006% |
| 1.20 fixture invoice_012.html | 26.335 | 26.311 | -0.091% | +0.011% |
| 1.20 fixture statement_012.html | 25.657 | 25.863 | +0.803% | -0.040% |
| 1.20 fixture multi_page_report_012.html | 27.527 | 27.858 | +1.202% | +0.031% |
| 1.20 fixture purchase_order.html | 40.486 | 40.183 | -0.748% | +0.018% |
| 1.20 fixture material_requisition.html | 51.116 | 51.065 | -0.100% | -0.012% |
| 1.20 fixture government_application_form.html | 38.079 | 37.762 | -0.832% | +0.008% |
| 1.20 fixture stock_sticker.html | 11.512 | 11.551 | +0.339% | +0.010% |
| 1.20 fixture trim_card.html | 48.612 | 48.601 | -0.023% | -0.013% |
| 1.20 100-row HTML render | 132.897 | 133.078 | +0.136% | -0.007% |
| 1.20 100-row uncompressed full-font render | 119.981 | 119.777 | -0.170% | +0.007% |
| 1.20 100-row PDF text extraction | 65.072 | 64.849 | -0.343% | -0.019% |
| 1.20 100-row PDF visual text extraction | 66.572 | 66.166 | -0.610% | +0.009% |
| 1.20 repeated text layout | 25.411 | 25.448 | +0.146% | +0.009% |
| 1.20 long paragraph layout | 43.896 | 44.012 | +0.264% | +0.007% |
| 1.20 five repeated PNG images | 13.473 | 13.484 | +0.082% | +0.003% |
| 1.20 ten repeated 512px PNG images | 322.156 | 322.436 | +0.087% | +0.004% |
| 1.20 800-word inline paragraph | 40.133 | 40.370 | +0.591% | -0.005% |
| 1.20 160 table headings | 54.394 | 54.038 | -0.654% | +0.055% |
| 1.20 4,000 boxes on one page | 11.790 | 11.836 | +0.390% | -0.007% |
| 1.20 100 ordinary Text-field fill | 344.148 | 344.365 | +0.063% | +0.003% |
| 1.20 100 ordinary Text-field flatten | 26.593 | 26.733 | +0.526% | +0.000% |

The paired bootstrap review uses mean log time ratios, 10,000 resamples and
seed 4505. All samples, intervals and any longer repeats are retained locally.
Primary positive intervals for the Elixir 1.19 multi-page report and stock
sticker receive three longer 61-sample alternating repetitions each. All six
paired bootstrap intervals include zero. The report medians change -0.174%,
-0.018% and +0.196%. Stock-sticker medians remain +0.124%, +0.323% and
+0.451%, with absolute differences of 15, 39 and 55 microseconds. These higher
measurements are retained; they do not establish an increase beyond the observed
variation. The repetition review uses 10,000 paired log resamples, seed 4606.
This adjacent acceptance does not close the cumulative fixed-baseline audit.

| Runtime / repeat | B45 ms | B46 ms | Time change | Paired log 95% interval |
| --- | ---: | ---: | ---: | ---: |
| 1.19 repeat-1-multi-page-report | 28.182 | 28.133 | -0.174% | [-0.041%, +1.285%] |
| 1.19 repeat-1-stock-sticker | 12.128 | 12.143 | +0.124% | [-0.771%, +1.219%] |
| 1.19 repeat-2-multi-page-report | 28.334 | 28.329 | -0.018% | [-0.483%, +0.864%] |
| 1.19 repeat-2-stock-sticker | 12.072 | 12.111 | +0.323% | [-0.688%, +1.497%] |
| 1.19 repeat-3-multi-page-report | 28.502 | 28.558 | +0.196% | [-0.904%, +0.621%] |
| 1.19 repeat-3-stock-sticker | 12.185 | 12.240 | +0.451% | [-0.812%, +1.215%] |


### Feature scaling and memory

Feature workloads use last-authored selections, first-enabled defaults or
increasing option lists. Dropdown and option counts are 1, 100 and 1,000 in both
static and interactive mode. Each PDF must exactly equal the accepted B45 PDF
for equivalent markup with effective selectedness authored explicitly. Captions,
field values, choice counts and page count are checked separately. Each case has
three warmups and seven fresh-worker timing samples. A first operation per case
is also retained; it is not a cold application-startup measurement. Three separate
whole-VM memory runs per case sample at 10 ms, include loaded code and retained
inputs/output, and exclude native allocations and brief peaks. They are not
isolated renderer-process memory measurements.

| Runtime / family | Input count / controls | Mode | Median ms | Reductions | Pages | Largest sampled VM MiB | PDF bytes |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 1.19 last_selected | 1 / 1 | static | 10.470 | 949238 | 1 | 67.51 | 77759 |
| 1.19 last_selected | 1 / 1 | interactive | 17.999 | 1362343 | 1 | 85.01 | 154775 |
| 1.19 last_selected | 100 / 100 | static | 27.189 | 2584755 | 4 | 78.57 | 82673 |
| 1.19 last_selected | 100 / 100 | interactive | 47.964 | 4369661 | 4 | 88.83 | 251066 |
| 1.19 last_selected | 1000 / 1000 | static | 248.910 | 17869936 | 40 | 119.71 | 113068 |
| 1.19 last_selected | 1000 / 1000 | interactive | 439.393 | 33660331 | 40 | 146.55 | 1137773 |
| 1.19 enabled_default | 1 / 1 | static | 9.950 | 949723 | 1 | 72.54 | 77759 |
| 1.19 enabled_default | 1 / 1 | interactive | 17.528 | 1363621 | 1 | 72.52 | 154775 |
| 1.19 enabled_default | 100 / 100 | static | 27.094 | 2571196 | 4 | 77.31 | 82673 |
| 1.19 enabled_default | 100 / 100 | interactive | 46.840 | 4358617 | 4 | 89.08 | 251066 |
| 1.19 enabled_default | 1000 / 1000 | static | 255.372 | 17772023 | 40 | 108.90 | 113068 |
| 1.19 enabled_default | 1000 / 1000 | interactive | 447.068 | 33563436 | 40 | 145.55 | 1137773 |
| 1.19 option_count | 1 / 1 | static | 8.240 | 936523 | 1 | 71.61 | 77659 |
| 1.19 option_count | 1 / 1 | interactive | 14.634 | 1373154 | 1 | 65.29 | 154896 |
| 1.19 option_count | 100 / 1 | static | 10.411 | 1057596 | 1 | 73.17 | 77775 |
| 1.19 option_count | 100 / 1 | interactive | 17.960 | 1546904 | 1 | 74.41 | 156881 |
| 1.19 option_count | 1000 / 1 | static | 25.062 | 2168610 | 1 | 75.52 | 77779 |
| 1.19 option_count | 1000 / 1 | interactive | 36.832 | 3205258 | 1 | 74.97 | 175788 |
| 1.20 last_selected | 1 / 1 | static | 10.548 | 944713 | 1 | 71.78 | 77759 |
| 1.20 last_selected | 1 / 1 | interactive | 17.511 | 1347211 | 1 | 67.47 | 154775 |
| 1.20 last_selected | 100 / 100 | static | 26.581 | 2410470 | 4 | 74.38 | 82673 |
| 1.20 last_selected | 100 / 100 | interactive | 45.144 | 3974562 | 4 | 85.39 | 251066 |
| 1.20 last_selected | 1000 / 1000 | static | 249.018 | 16129224 | 40 | 120.89 | 113068 |
| 1.20 last_selected | 1000 / 1000 | interactive | 414.059 | 29838775 | 40 | 153.71 | 1137773 |
| 1.20 enabled_default | 1 / 1 | static | 10.450 | 944460 | 1 | 70.71 | 77759 |
| 1.20 enabled_default | 1 / 1 | interactive | 17.863 | 1346755 | 1 | 75.85 | 154775 |
| 1.20 enabled_default | 100 / 100 | static | 26.204 | 2398827 | 4 | 74.07 | 82673 |
| 1.20 enabled_default | 100 / 100 | interactive | 45.043 | 3965090 | 4 | 89.44 | 251066 |
| 1.20 enabled_default | 1000 / 1000 | static | 236.858 | 15956695 | 40 | 122.14 | 113068 |
| 1.20 enabled_default | 1000 / 1000 | interactive | 428.691 | 29760823 | 40 | 148.94 | 1137773 |
| 1.20 option_count | 1 / 1 | static | 8.226 | 932368 | 1 | 64.32 | 77659 |
| 1.20 option_count | 1 / 1 | interactive | 14.592 | 1351627 | 1 | 70.40 | 154896 |
| 1.20 option_count | 100 / 1 | static | 10.346 | 1045683 | 1 | 64.32 | 77775 |
| 1.20 option_count | 100 / 1 | interactive | 17.523 | 1522762 | 1 | 69.52 | 156881 |
| 1.20 option_count | 1000 / 1 | static | 28.619 | 2135796 | 1 | 76.52 | 77779 |
| 1.20 option_count | 1000 / 1 | interactive | 36.364 | 3151292 | 1 | 75.16 | 175788 |

Ordinary memory checks retain all 21 exact operations and compare complete output
hashes across three runs per revision. The same sampling limitations apply.

| Runtime / workload | Largest B45 VM MiB | Largest B46 VM MiB | Output bytes |
| --- | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 136.06 | 80.63 | 162435 |
| 1.19 fixture statement_012.html | 80.56 | 80.54 | 161286 |
| 1.19 fixture multi_page_report_012.html | 78.94 | 78.93 | 162770 |
| 1.19 fixture purchase_order.html | 83.58 | 92.47 | 124909 |
| 1.19 fixture material_requisition.html | 98.67 | 85.57 | 126178 |
| 1.19 fixture government_application_form.html | 78.92 | 78.89 | 250479 |
| 1.19 fixture stock_sticker.html | 77.18 | 77.17 | 59392 |
| 1.19 fixture trim_card.html | 92.18 | 92.25 | 163615 |
| 1.19 100-row HTML render | 92.97 | 88.06 | 166773 |
| 1.19 100-row uncompressed full-font render | 86.69 | 86.41 | 1630334 |
| 1.19 100-row PDF text extraction | 98.38 | 98.41 | 9171 |
| 1.19 100-row PDF visual text extraction | 87.74 | 87.73 | 23286 |
| 1.19 repeated text layout | 79.38 | 79.40 | 82140 |
| 1.19 long paragraph layout | 85.37 | 85.35 | 85076 |
| 1.19 five repeated PNG images | 77.16 | 77.19 | 37811 |
| 1.19 ten repeated 512px PNG images | 94.95 | 90.18 | 634859 |
| 1.19 800-word inline paragraph | 93.48 | 93.46 | 84761 |
| 1.19 160 table headings | 96.02 | 88.48 | 155020 |
| 1.19 4,000 boxes on one page | 78.91 | 78.82 | 21226 |
| 1.19 100 ordinary Text-field fill | 268.27 | 268.27 | 308620 |
| 1.19 100 ordinary Text-field flatten | 76.80 | 76.79 | 316422 |
| 1.20 fixture invoice_012.html | 154.03 | 90.76 | 162435 |
| 1.20 fixture statement_012.html | 81.37 | 90.73 | 161286 |
| 1.20 fixture multi_page_report_012.html | 77.79 | 77.76 | 162770 |
| 1.20 fixture purchase_order.html | 82.92 | 96.23 | 124909 |
| 1.20 fixture material_requisition.html | 81.06 | 90.72 | 126178 |
| 1.20 fixture government_application_form.html | 82.95 | 82.95 | 250479 |
| 1.20 fixture stock_sticker.html | 76.13 | 76.13 | 59392 |
| 1.20 fixture trim_card.html | 91.94 | 91.91 | 163615 |
| 1.20 100-row HTML render | 86.51 | 86.52 | 166773 |
| 1.20 100-row uncompressed full-font render | 85.53 | 85.81 | 1630334 |
| 1.20 100-row PDF text extraction | 84.27 | 84.27 | 9171 |
| 1.20 100-row PDF visual text extraction | 86.69 | 84.27 | 23286 |
| 1.20 repeated text layout | 78.27 | 78.28 | 82140 |
| 1.20 long paragraph layout | 81.03 | 81.09 | 85076 |
| 1.20 five repeated PNG images | 76.16 | 76.17 | 37811 |
| 1.20 ten repeated 512px PNG images | 91.16 | 91.22 | 634859 |
| 1.20 800-word inline paragraph | 82.75 | 85.79 | 84761 |
| 1.20 160 table headings | 94.91 | 94.90 | 155020 |
| 1.20 4,000 boxes on one page | 78.28 | 78.24 | 21226 |
| 1.20 100 ordinary Text-field fill | 267.18 | 267.25 | 308620 |
| 1.20 100 ordinary Text-field flatten | 74.87 | 74.87 | 316422 |

### Validation and remaining work

The focused static fixture preserves its captions, selected text and one page,
with 0.257% changed pixels against scripting-disabled Chromium under the unchanged
1% limit. Native dropdown/control artwork still differs from Chromium's skin. Coverage includes empty/default/last-authored selection, disabled
options, dormant templates, raw attributes, checked and negated checked selectors,
PDF values, fill/flatten behavior and public malformed-state diagnostics.
The full quality matrix passes both supported runtimes with 1,341 unit tests,
100% coverage and all 127 browser comparisons per runtime. There are no WARN
or FAIL entries. Logs: `.quality/logs/20261011T022917Z-939224`. The exact host browser
command also passes all 127 comparisons.

All 17,267 inventory IDs are retained. Checked selector and checkedness definitions
move from planned to partial; broader select/option rows remain partial. Inventory
counts are 2,254 implemented, 4,698 partial, 2,197 planned, 421 printable exclusions
and 7,697 non-printing records. Classification accounting is complete;
implementation coverage is not. Full 1.1.0 implementation and the cumulative
release-performance audit remain open.

All benchmark reports are ignored under .quality/performance, with consolidated
bundles and original report bytes/digests preserved. No benchmark JSON is tracked.


## Batch 47: option labels, groups and intrinsic height

Ordinary single-select dropdowns now collect options in group order and inherit
optgroup disabled state. A nonempty option label attribute supplies the display
label independently of its export value. Empty or absent labels fall back to
ASCII-trimmed and collapsed option text. Explicit values remain intact; absent
values use normalized text independently of the label. NBSP remains intact.
Canonical choices and states drive static artwork, PDF values and selectors.
Raw attributes are retained. Dormant template and data-script children contribute
no choices. Group label metadata and omitted group endings are supported;
invalid group structure uses the shared actionable diagnostics.

Empty, blank-option and all-disabled unselected dropdowns reserve the same font
line as populated dropdowns without an authored height. Native dropdown style
resets authored line-height to normal under the pinned HTML select rendering
rules. Native dropdown text reserves one CSS pixel of internal leading above and below
the existing normal font line, independently of authored padding. Ordinary text
line-height behavior is unchanged. The fixture initially exposed
2.267% changed pixels; resetting line-height alone still produced 2.533% because
the native internal leading was missing. An outward font-rounding attempt passed
the isolated 10pt fixture but regressed the government form to 3.221% changed
pixels. It was rejected; the final rule preserves the government form output
exactly. The failed capture log and geometry findings are retained under .quality. The final fixture retains all seven difficult cases,
automatic height and unchanged thresholds.

Five focused tests extend existing test files. Layout coverage spans block,
inline-block, flex and grid, two font sizes, fixed/multiplier/normal line-height,
and empty/blank/populated/all-disabled options. Remaining controls work includes
multiple/listbox sizing, rich/custom options and groups, individual disabled-option
policy in editable PDFs, constraints and shadow integration. Broad control records
remain partial. The full 1.1.0 plan is unfinished.

### Ordinary workload measurements

Both supported runtimes use two BEAM schedulers with CPU affinity 4,5. The frozen
19 ordinary operations and two form fill/flatten operations retain B46 inputs,
options and assets. Every complete output must remain byte-identical to B46.
There are 31 alternating samples per revision, five warmups and fresh sample
workers after one untimed operation and GC. Module loading is outside timing.
The fixed fee6c4d baseline remains preserved and its cumulative release audit is
still open. Bootstrap review uses paired log time ratios, 10,000 resamples and
seed 4505. Timing intervals guide longer repetitions and do not replace the
existing guardrails.

| Runtime / workload | B46 ms | B47 ms | Time change | Reduction change |
| --- | ---: | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 27.050 | 26.781 | -0.994% | -0.039% |
| 1.19 fixture statement_012.html | 26.518 | 26.634 | +0.437% | +0.018% |
| 1.19 fixture multi_page_report_012.html | 28.393 | 28.477 | +0.296% | +0.061% |
| 1.19 fixture purchase_order.html | 39.669 | 39.945 | +0.696% | +0.005% |
| 1.19 fixture material_requisition.html | 52.484 | 52.375 | -0.208% | +0.027% |
| 1.19 fixture government_application_form.html | 38.928 | 38.808 | -0.308% | +0.011% |
| 1.19 fixture stock_sticker.html | 12.148 | 12.053 | -0.782% | -0.032% |
| 1.19 fixture trim_card.html | 48.707 | 48.873 | +0.341% | -0.001% |
| 1.19 100-row HTML render | 145.240 | 145.175 | -0.045% | +0.013% |
| 1.19 100-row uncompressed full-font render | 128.829 | 129.116 | +0.223% | +0.033% |
| 1.19 100-row PDF text extraction | 65.605 | 66.106 | +0.764% | +0.007% |
| 1.19 100-row PDF visual text extraction | 67.600 | 67.584 | -0.024% | -0.006% |
| 1.19 repeated text layout | 26.105 | 26.151 | +0.176% | +0.000% |
| 1.19 long paragraph layout | 49.991 | 50.121 | +0.260% | +0.005% |
| 1.19 five repeated PNG images | 13.508 | 13.496 | -0.089% | +0.001% |
| 1.19 ten repeated 512px PNG images | 326.967 | 326.308 | -0.202% | -0.001% |
| 1.19 800-word inline paragraph | 42.665 | 42.886 | +0.518% | +0.017% |
| 1.19 160 table headings | 53.582 | 53.622 | +0.075% | +0.008% |
| 1.19 4,000 boxes on one page | 14.168 | 14.187 | +0.134% | -0.008% |
| 1.19 100 ordinary Text-field fill | 352.446 | 352.395 | -0.014% | +0.003% |
| 1.19 100 ordinary Text-field flatten | 28.437 | 28.258 | -0.629% | -0.003% |
| 1.20 fixture invoice_012.html | 26.265 | 26.428 | +0.621% | +0.016% |
| 1.20 fixture statement_012.html | 25.931 | 25.919 | -0.046% | -0.009% |
| 1.20 fixture multi_page_report_012.html | 27.426 | 27.433 | +0.026% | -0.014% |
| 1.20 fixture purchase_order.html | 38.233 | 38.397 | +0.429% | -0.031% |
| 1.20 fixture material_requisition.html | 51.274 | 50.837 | -0.852% | -0.016% |
| 1.20 fixture government_application_form.html | 38.297 | 38.137 | -0.418% | +0.012% |
| 1.20 fixture stock_sticker.html | 11.589 | 11.533 | -0.483% | +0.009% |
| 1.20 fixture trim_card.html | 48.224 | 48.260 | +0.075% | +0.009% |
| 1.20 100-row HTML render | 131.895 | 132.521 | +0.475% | +0.008% |
| 1.20 100-row uncompressed full-font render | 120.306 | 119.911 | -0.328% | -0.043% |
| 1.20 100-row PDF text extraction | 64.833 | 65.403 | +0.879% | +0.013% |
| 1.20 100-row PDF visual text extraction | 67.407 | 66.580 | -1.227% | -0.010% |
| 1.20 repeated text layout | 26.177 | 26.115 | -0.237% | +0.001% |
| 1.20 long paragraph layout | 43.404 | 43.488 | +0.194% | +0.009% |
| 1.20 five repeated PNG images | 13.470 | 13.435 | -0.260% | +0.001% |
| 1.20 ten repeated 512px PNG images | 322.081 | 319.938 | -0.665% | -0.007% |
| 1.20 800-word inline paragraph | 40.189 | 40.186 | -0.007% | -0.017% |
| 1.20 160 table headings | 52.915 | 52.652 | -0.497% | +0.013% |
| 1.20 4,000 boxes on one page | 11.577 | 11.727 | +1.296% | +0.008% |
| 1.20 100 ordinary Text-field fill | 344.747 | 345.331 | +0.169% | +0.003% |
| 1.20 100 ordinary Text-field flatten | 26.718 | 26.499 | -0.820% | +0.000% |

Primary positive paired bootstrap intervals:

- 1.20.2 100 ordinary Text-field fill: +0.169% median, interval [0.13592027191247488, 1.3635116608164848].

Longer repeat evidence:

| Runtime / repeat | B46 ms | B47 ms | Time change | Paired log 95% interval |
| --- | ---: | ---: | ---: | ---: |
| 1.20.2 repeat-1-text-fill-elixir-1.20.json | 344.803 | 345.608 | +0.233% | [-0.21111032809428476, 0.4021358956138808] |
| 1.20.2 repeat-2-text-fill-elixir-1.20.json | 345.007 | 344.840 | -0.048% | [-0.35112781966371204, 0.25169760095709603] |
| 1.20.2 repeat-3-text-fill-elixir-1.20.json | 343.955 | 344.849 | +0.260% | [-0.18585492743836884, 0.3864393158278223] |

All three longer-repeat paired 95% intervals include zero. The medians change
+0.233%, -0.048% and +0.260%, corresponding to +0.805 ms, -0.167 ms and
+0.894 ms. The measurements do not establish a persistent increase beyond the
observed variation. Higher medians and every original capture remain recorded;
the cumulative fixed-baseline audit is still open.

### Feature scaling and memory

Four families cover disabled groups, separate labels/text values, increasing
option/group lists and automatic empty dropdown height. Counts are 1, 100 and
1,000 in static and interactive mode. Each complete PDF must equal its B46
reference for equivalent flat markup and explicit canonical labels/values.
Empty references declare the normal font content height, while candidate height
remains automatic. Captions, field values, choice counts and pages are checked.
Each case retains its first operation, three warmups and seven fresh-worker
samples after one untimed operation and GC. First operations are not cold
application startup measurements. Three separate whole-VM memory runs sample
at 10 ms. They include loaded code and retained input/output, exclude native
allocations and brief peaks, and do not isolate renderer-process memory.

| Runtime / family | Input count / controls | Mode | Median ms | Reductions | Pages | Largest sampled VM MiB | PDF bytes |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 1.19 disabled_groups | 1 / 1 | static | 8.567 | 940258 | 1 | 71.58 | 78011 |
| 1.19 disabled_groups | 1 / 1 | interactive | 15.418 | 1380570 | 1 | 70.94 | 155161 |
| 1.19 disabled_groups | 100 / 100 | static | 33.590 | 3078212 | 5 | 77.43 | 83798 |
| 1.19 disabled_groups | 100 / 100 | interactive | 54.734 | 4890781 | 5 | 90.04 | 255619 |
| 1.19 disabled_groups | 1000 / 1000 | static | 298.018 | 22860256 | 42 | 123.40 | 118989 |
| 1.19 disabled_groups | 1000 / 1000 | interactive | 505.734 | 38842097 | 42 | 148.85 | 1160990 |
| 1.19 label_and_text | 1 / 1 | static | 10.350 | 949614 | 1 | 64.40 | 78011 |
| 1.19 label_and_text | 1 / 1 | interactive | 17.792 | 1367250 | 1 | 66.60 | 155169 |
| 1.19 label_and_text | 100 / 100 | static | 28.201 | 2743514 | 5 | 79.90 | 83798 |
| 1.19 label_and_text | 100 / 100 | interactive | 49.368 | 4610937 | 5 | 76.28 | 256603 |
| 1.19 label_and_text | 1000 / 1000 | static | 256.611 | 19683430 | 42 | 120.64 | 118989 |
| 1.19 label_and_text | 1000 / 1000 | interactive | 467.868 | 36291995 | 42 | 136.11 | 1172776 |
| 1.19 option_count | 1 / 1 | static | 8.774 | 941542 | 1 | 71.07 | 77739 |
| 1.19 option_count | 1 / 1 | interactive | 15.015 | 1374287 | 1 | 66.60 | 154908 |
| 1.19 option_count | 100 / 1 | static | 12.121 | 1178738 | 1 | 67.95 | 77903 |
| 1.19 option_count | 100 / 1 | interactive | 19.837 | 1683888 | 1 | 69.22 | 157303 |
| 1.19 option_count | 1000 / 1 | static | 43.152 | 3391974 | 1 | 75.88 | 77907 |
| 1.19 option_count | 1000 / 1 | interactive | 54.513 | 4581535 | 1 | 81.22 | 179810 |
| 1.19 empty_height | 1 / 1 | static | 8.368 | 934122 | 1 | 66.46 | 77133 |
| 1.19 empty_height | 1 / 1 | interactive | 8.868 | 985395 | 1 | 65.40 | 78346 |
| 1.19 empty_height | 100 / 100 | static | 23.439 | 2310311 | 4 | 78.45 | 81628 |
| 1.19 empty_height | 100 / 100 | interactive | 31.719 | 3561763 | 4 | 81.69 | 164407 |
| 1.19 empty_height | 1000 / 1000 | static | 169.863 | 14862881 | 38 | 101.45 | 107983 |
| 1.19 empty_height | 1000 / 1000 | interactive | 364.167 | 28854573 | 38 | 142.42 | 957722 |
| 1.20 disabled_groups | 1 / 1 | static | 9.039 | 939666 | 1 | 71.42 | 78011 |
| 1.20 disabled_groups | 1 / 1 | interactive | 14.990 | 1359270 | 1 | 71.48 | 155161 |
| 1.20 disabled_groups | 100 / 100 | static | 30.044 | 2888381 | 5 | 79.46 | 83798 |
| 1.20 disabled_groups | 100 / 100 | interactive | 47.433 | 4476475 | 5 | 84.49 | 255619 |
| 1.20 disabled_groups | 1000 / 1000 | static | 273.901 | 20867323 | 42 | 126.27 | 118989 |
| 1.20 disabled_groups | 1000 / 1000 | interactive | 476.625 | 34822430 | 42 | 139.46 | 1160990 |
| 1.20 label_and_text | 1 / 1 | static | 10.026 | 944628 | 1 | 73.19 | 78011 |
| 1.20 label_and_text | 1 / 1 | interactive | 17.604 | 1350255 | 1 | 68.98 | 155169 |
| 1.20 label_and_text | 100 / 100 | static | 27.525 | 2577527 | 5 | 75.68 | 83798 |
| 1.20 label_and_text | 100 / 100 | interactive | 46.550 | 4226559 | 5 | 74.33 | 256603 |
| 1.20 label_and_text | 1000 / 1000 | static | 255.388 | 17888765 | 42 | 118.78 | 118989 |
| 1.20 label_and_text | 1000 / 1000 | interactive | 439.402 | 32416025 | 42 | 158.35 | 1172776 |
| 1.20 option_count | 1 / 1 | static | 8.387 | 937630 | 1 | 71.67 | 77739 |
| 1.20 option_count | 1 / 1 | interactive | 14.839 | 1356901 | 1 | 67.20 | 154908 |
| 1.20 option_count | 100 / 1 | static | 12.694 | 1176089 | 1 | 67.11 | 77903 |
| 1.20 option_count | 100 / 1 | interactive | 20.441 | 1663671 | 1 | 69.04 | 157303 |
| 1.20 option_count | 1000 / 1 | static | 43.852 | 3330123 | 1 | 72.68 | 77907 |
| 1.20 option_count | 1000 / 1 | interactive | 56.982 | 4506937 | 1 | 76.98 | 179810 |
| 1.20 empty_height | 1 / 1 | static | 8.318 | 933778 | 1 | 67.01 | 77133 |
| 1.20 empty_height | 1 / 1 | interactive | 8.811 | 972954 | 1 | 68.58 | 78346 |
| 1.20 empty_height | 100 / 100 | static | 22.368 | 2153612 | 4 | 79.55 | 81628 |
| 1.20 empty_height | 100 / 100 | interactive | 30.723 | 3202203 | 4 | 81.03 | 164407 |
| 1.20 empty_height | 1000 / 1000 | static | 186.465 | 13389602 | 38 | 110.99 | 107983 |
| 1.20 empty_height | 1000 / 1000 | interactive | 352.621 | 25344835 | 38 | 146.19 | 957722 |

All 21 ordinary operations also retain three whole-VM samples per revision,
with identical output hashes. The same sampling limitations apply.

| Runtime / workload | Largest B46 VM MiB | Largest B47 VM MiB | Output bytes |
| --- | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 145.43 | 80.09 | 162438 |
| 1.19 fixture statement_012.html | 79.15 | 79.71 | 161286 |
| 1.19 fixture multi_page_report_012.html | 86.38 | 78.12 | 162773 |
| 1.19 fixture purchase_order.html | 91.61 | 91.61 | 124908 |
| 1.19 fixture material_requisition.html | 94.64 | 90.90 | 126176 |
| 1.19 fixture government_application_form.html | 83.85 | 78.01 | 250478 |
| 1.19 fixture stock_sticker.html | 80.62 | 73.10 | 59392 |
| 1.19 fixture trim_card.html | 90.98 | 90.85 | 163622 |
| 1.19 100-row HTML render | 92.56 | 108.81 | 166773 |
| 1.19 100-row uncompressed full-font render | 87.98 | 83.77 | 1630334 |
| 1.19 100-row PDF text extraction | 110.32 | 97.48 | 9171 |
| 1.19 100-row PDF visual text extraction | 97.52 | 86.84 | 23286 |
| 1.19 repeated text layout | 81.99 | 82.34 | 82140 |
| 1.19 long paragraph layout | 90.95 | 92.35 | 85076 |
| 1.19 five repeated PNG images | 82.94 | 76.32 | 37811 |
| 1.19 ten repeated 512px PNG images | 91.00 | 94.18 | 634859 |
| 1.19 800-word inline paragraph | 98.78 | 91.48 | 84761 |
| 1.19 160 table headings | 102.74 | 83.63 | 155020 |
| 1.19 4,000 boxes on one page | 82.30 | 78.01 | 21226 |
| 1.19 100 ordinary Text-field fill | 265.79 | 265.81 | 308620 |
| 1.19 100 ordinary Text-field flatten | 132.33 | 75.97 | 316422 |
| 1.20 fixture invoice_012.html | 167.45 | 156.48 | 162438 |
| 1.20 fixture statement_012.html | 103.74 | 93.55 | 161286 |
| 1.20 fixture multi_page_report_012.html | 84.83 | 80.62 | 162773 |
| 1.20 fixture purchase_order.html | 96.46 | 102.33 | 124908 |
| 1.20 fixture material_requisition.html | 97.05 | 92.03 | 126176 |
| 1.20 fixture government_application_form.html | 89.47 | 85.57 | 250478 |
| 1.20 fixture stock_sticker.html | 79.91 | 79.06 | 59392 |
| 1.20 fixture trim_card.html | 94.76 | 94.71 | 163622 |
| 1.20 100-row HTML render | 98.90 | 88.67 | 166773 |
| 1.20 100-row uncompressed full-font render | 100.75 | 99.82 | 1630334 |
| 1.20 100-row PDF text extraction | 100.11 | 87.08 | 9171 |
| 1.20 100-row PDF visual text extraction | 91.27 | 93.57 | 23286 |
| 1.20 repeated text layout | 87.96 | 87.93 | 82140 |
| 1.20 long paragraph layout | 90.92 | 83.65 | 85076 |
| 1.20 five repeated PNG images | 85.58 | 78.97 | 37811 |
| 1.20 ten repeated 512px PNG images | 94.62 | 95.07 | 634859 |
| 1.20 800-word inline paragraph | 100.16 | 94.76 | 84761 |
| 1.20 160 table headings | 105.51 | 97.69 | 155020 |
| 1.20 4,000 boxes on one page | 89.28 | 81.04 | 21226 |
| 1.20 100 ordinary Text-field fill | 268.46 | 268.41 | 308620 |
| 1.20 100 ordinary Text-field flatten | 132.24 | 77.68 | 316422 |

### Validation and remaining work

The static option fixture checks captions, selected labels, visibility and one
page in addition to the unchanged 1% changed-pixel threshold. Native dropdown
skin and arrow artwork still differ from Chromium. The full quality matrix passes both supported runtimes with 1,346 unit tests,
100% coverage and all 128 browser comparisons per runtime. There are no WARN
or FAIL entries. Logs: `.quality/logs/20261011T032042Z-1031090`. The exact host browser
command also passes all 128 comparisons.

Inventory auditing retains all 17,267 IDs. Optgroup and group-disabled records
move from planned to partial; broad option/select records remain partial.
Counts are 2,254 implemented, 4,700 partial, 2,195 planned, 421 printable exclusions
and 7,697 non-printing records. This accounting does not establish complete
implementation. Full 1.1.0 and the cumulative release performance audit remain
open. Benchmark JSON stays ignored under .quality/performance, consolidated
with original report bytes and SHA256 digests preserved.


## Batch 48: initial closed popovers

Popover values retain their authored spelling. Empty is Auto; ASCII-insensitive
auto, hint and manual resolve to their named states; missing is None; invalid
values are Manual without whitespace trimming. All initial popovers remain
closed in the scripting-disabled profile. Static :popover-open never matches,
including escaped and uppercase selector names; negation matches ordinary
closed elements. Closed paint, fields and unused own/descendant images and
backgrounds are omitted. Actual sibling positions and initial DOM radio-group
selection remain. A closed checked radio can uncheck a visible earlier radio.
The existing hidden-input UA important rule still wins over author display.

Author display overrides are not suppressed. They receive an actionable
unsupported_html diagnostic because native fixed positioning and fit-content
geometry are unfinished. Activation, top-layer/dialog composition and complete
popover geometry remain open. This is an initial-state foundation, not complete
popover support. Broad popover inventory records remain partial.

HTML and supplied DOM validation still traverses closed content. Executable
scripts, event handlers and active javascript/vbscript href, src and xlink:href
references diagnose before hiding. Unregistered custom href/src attributes
remain metadata because they do not create URL behavior. URL matching accounts for ASCII case,
leading URL controls/spaces and embedded tabs/newlines. CSS background references
use existing shared escape decoding and the same active-scheme validator.
Background reference syntax validation moves from Style to HtmlValidator.
Ordinary local-resource and link authorization retain their existing rules.
Seven focused tests extend existing files. New public failure tests assert shared
reason/stage/message/source and render context. No test modules are added.

### Ordinary workload measurements

Both supported runtimes retain all 21 B47 operations, inputs, options and assets.
Every complete ordinary output must be byte-identical to B47. There are 31
alternating samples per revision, five warmups and fresh timing workers after
one untimed operation and GC. Two BEAM schedulers use CPU affinity 4,5; module
loading stays outside timing. Paired log-ratio bootstrap review uses 10,000
resamples and seed 4505. Statistical intervals guide repetitions and do not
replace profiling or unchanged guardrails. The fixed fee6c4d baseline remains
preserved and its cumulative release audit remains open.

| Runtime / workload | B47 ms | B48 ms | Time change | Reduction change |
| --- | ---: | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 26.935 | 26.913 | -0.082% | +0.027% |
| 1.19 fixture statement_012.html | 25.951 | 25.980 | +0.112% | +0.027% |
| 1.19 fixture multi_page_report_012.html | 28.433 | 28.128 | -1.073% | -0.042% |
| 1.19 fixture purchase_order.html | 39.629 | 40.014 | +0.972% | +0.082% |
| 1.19 fixture material_requisition.html | 52.630 | 52.884 | +0.483% | +0.013% |
| 1.19 fixture government_application_form.html | 39.169 | 39.054 | -0.294% | +0.006% |
| 1.19 fixture stock_sticker.html | 11.557 | 11.577 | +0.173% | +0.016% |
| 1.19 fixture trim_card.html | 49.588 | 49.695 | +0.216% | +0.050% |
| 1.19 100-row HTML render | 144.777 | 144.524 | -0.175% | +0.114% |
| 1.19 100-row uncompressed full-font render | 134.282 | 132.694 | -1.183% | +0.100% |
| 1.19 100-row PDF text extraction | 65.116 | 65.821 | +1.083% | -0.004% |
| 1.19 100-row PDF visual text extraction | 67.351 | 67.446 | +0.141% | +0.017% |
| 1.19 repeated text layout | 26.314 | 26.397 | +0.315% | +0.027% |
| 1.19 long paragraph layout | 50.167 | 50.195 | +0.056% | +0.006% |
| 1.19 five repeated PNG images | 13.471 | 13.504 | +0.245% | +0.023% |
| 1.19 ten repeated 512px PNG images | 325.505 | 327.124 | +0.497% | +0.005% |
| 1.19 800-word inline paragraph | 42.751 | 42.425 | -0.763% | -0.017% |
| 1.19 160 table headings | 53.638 | 53.556 | -0.153% | +0.295% |
| 1.19 4,000 boxes on one page | 14.129 | 14.204 | +0.531% | -0.004% |
| 1.19 100 ordinary Text-field fill | 348.562 | 348.236 | -0.094% | -0.001% |
| 1.19 100 ordinary Text-field flatten | 28.417 | 28.274 | -0.503% | -0.003% |
| 1.20 fixture invoice_012.html | 26.464 | 26.213 | -0.948% | +0.033% |
| 1.20 fixture statement_012.html | 25.737 | 25.899 | +0.629% | +0.067% |
| 1.20 fixture multi_page_report_012.html | 27.744 | 27.826 | +0.296% | +0.019% |
| 1.20 fixture purchase_order.html | 38.389 | 38.393 | +0.010% | +0.067% |
| 1.20 fixture material_requisition.html | 50.763 | 50.859 | +0.189% | +0.019% |
| 1.20 fixture government_application_form.html | 38.853 | 38.804 | -0.126% | +0.035% |
| 1.20 fixture stock_sticker.html | 11.406 | 11.471 | +0.570% | +0.030% |
| 1.20 fixture trim_card.html | 48.916 | 49.005 | +0.182% | +0.045% |
| 1.20 100-row HTML render | 132.702 | 132.641 | -0.046% | +0.124% |
| 1.20 100-row uncompressed full-font render | 119.864 | 120.237 | +0.311% | +0.138% |
| 1.20 100-row PDF text extraction | 64.319 | 64.269 | -0.078% | +0.009% |
| 1.20 100-row PDF visual text extraction | 65.601 | 65.996 | +0.602% | +0.005% |
| 1.20 repeated text layout | 26.062 | 26.275 | +0.817% | +0.038% |
| 1.20 long paragraph layout | 43.463 | 43.677 | +0.492% | +0.000% |
| 1.20 five repeated PNG images | 13.461 | 13.467 | +0.045% | +0.024% |
| 1.20 ten repeated 512px PNG images | 322.750 | 322.141 | -0.189% | +0.003% |
| 1.20 800-word inline paragraph | 39.933 | 39.836 | -0.243% | -0.007% |
| 1.20 160 table headings | 53.840 | 53.488 | -0.654% | +0.250% |
| 1.20 4,000 boxes on one page | 11.549 | 11.711 | +1.403% | -0.001% |
| 1.20 100 ordinary Text-field fill | 344.953 | 347.146 | +0.636% | +0.003% |
| 1.20 100 ordinary Text-field flatten | 25.824 | 25.985 | +0.623% | +0.000% |

Primary comparisons with positive paired intervals:

- 1.20.2 100-row PDF visual text extraction: +0.602% median, interval [0.13047670262568162, 1.4514118459768655].
- 1.20.2 repeated text layout: +0.817% median, interval [0.23401494881114954, 1.5850777402988545].

Longer repeat evidence:

| Runtime / repeat | B47 ms | B48 ms | Time change | Paired 95% interval |
| --- | ---: | ---: | ---: | ---: |
| 1.20.2 repeat-1-100-row-pdf-visual-text-extraction-elixir-1.20.json | 66.296 | 66.236 | -0.091% | [-0.8191184456748503, 0.6770796974156114] |
| 1.20.2 repeat-1-repeated-text-layout-elixir-1.20.json | 26.088 | 26.057 | -0.119% | [-0.6683132073024827, 0.5335322369667485] |
| 1.20.2 repeat-2-100-row-pdf-visual-text-extraction-elixir-1.20.json | 65.327 | 65.279 | -0.073% | [-0.9569804266200537, 0.48684018506147103] |
| 1.20.2 repeat-2-repeated-text-layout-elixir-1.20.json | 25.885 | 26.054 | +0.653% | [-0.09225557846140571, 1.134426184943129] |
| 1.20.2 repeat-3-100-row-pdf-visual-text-extraction-elixir-1.20.json | 65.330 | 65.588 | +0.395% | [-0.610412707069179, 0.9530729844188105] |
| 1.20.2 repeat-3-repeated-text-layout-elixir-1.20.json | 26.040 | 26.012 | -0.108% | [-0.47957652494295583, 0.4266176838479648] |

Every primary comparison with a positive paired interval has three longer 61-sample repetitions. None of those repetitions establishes an increase beyond paired measurement variation. All primary/higher medians remain retained. The cumulative fixed-baseline audit remains open.

### Feature scaling and memory

Closed subtrees, own images and closed controls cover 1, 100 and 1,000 elements
in static and interactive mode. Each complete output must equal B47 equivalent
hidden markup with the same visible content and DOM controls. B47 resolves own
backgrounds before hiding, so references omit the unused background declaration.
B47 own-image references use a hidden ordinary wrapper to skip unused source
resolution. Resource content is retained; no ordinary input is altered. Failed
reference attempts and findings remain under .quality. The final reference
requirements are frozen before the ordinary captures.

Each case retains its first operation, three warmups, seven fresh-worker timing
samples and three separate whole-VM memory runs sampled at 10 ms. First operations
are not cold application startup. Memory includes loaded code and retained
inputs/output, excludes native allocations and shorter peaks, and does not
isolate renderer-process memory. Content, field counts, pages and complete output
hashes are checked independently of elapsed time.

| Runtime / family | Count | Mode | Median ms | Reductions | Pages | Largest sampled VM MiB | PDF bytes |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 1.19 closed_subtrees | 1 | static | 8.488 | 935370 | 1 | 70.08 | 77215 |
| 1.19 closed_subtrees | 1 | interactive | 8.532 | 935467 | 1 | 66.30 | 77215 |
| 1.19 closed_subtrees | 100 | static | 21.633 | 2159383 | 4 | 73.60 | 81039 |
| 1.19 closed_subtrees | 100 | interactive | 21.689 | 2163627 | 4 | 70.64 | 81039 |
| 1.19 closed_subtrees | 1000 | static | 144.836 | 13373996 | 32 | 82.54 | 96681 |
| 1.19 closed_subtrees | 1000 | interactive | 146.164 | 13388967 | 32 | 78.84 | 96681 |
| 1.19 own_images | 1 | static | 8.334 | 932876 | 1 | 67.33 | 77215 |
| 1.19 own_images | 1 | interactive | 8.290 | 932851 | 1 | 67.99 | 77215 |
| 1.19 own_images | 100 | static | 18.621 | 1952075 | 4 | 71.98 | 81039 |
| 1.19 own_images | 100 | interactive | 18.449 | 1951782 | 4 | 75.40 | 81039 |
| 1.19 own_images | 1000 | static | 106.556 | 11175474 | 32 | 91.20 | 96681 |
| 1.19 own_images | 1000 | interactive | 106.820 | 11188086 | 32 | 92.24 | 96681 |
| 1.19 closed_controls | 1 | static | 8.555 | 944774 | 1 | 66.46 | 78589 |
| 1.19 closed_controls | 1 | interactive | 14.908 | 1380624 | 1 | 73.24 | 155343 |
| 1.19 closed_controls | 100 | static | 31.960 | 3011891 | 7 | 78.42 | 85265 |
| 1.19 closed_controls | 100 | interactive | 49.533 | 4776117 | 7 | 82.96 | 256232 |
| 1.19 closed_controls | 1000 | static | 270.789 | 22227160 | 69 | 115.73 | 132150 |
| 1.19 closed_controls | 1000 | interactive | 447.534 | 37907684 | 69 | 138.90 | 1168017 |
| 1.20 closed_subtrees | 1 | static | 8.506 | 931163 | 1 | 69.18 | 77215 |
| 1.20 closed_subtrees | 1 | interactive | 8.333 | 931162 | 1 | 66.90 | 77215 |
| 1.20 closed_subtrees | 100 | static | 22.178 | 2084804 | 4 | 73.86 | 81039 |
| 1.20 closed_subtrees | 100 | interactive | 22.590 | 2086108 | 4 | 82.37 | 81039 |
| 1.20 closed_subtrees | 1000 | static | 143.604 | 12527158 | 32 | 79.89 | 96681 |
| 1.20 closed_subtrees | 1000 | interactive | 144.266 | 12540056 | 32 | 79.82 | 96681 |
| 1.20 own_images | 1 | static | 8.249 | 928964 | 1 | 71.62 | 77215 |
| 1.20 own_images | 1 | interactive | 8.307 | 929044 | 1 | 71.64 | 77215 |
| 1.20 own_images | 100 | static | 18.494 | 1879138 | 4 | 67.33 | 81039 |
| 1.20 own_images | 100 | interactive | 18.411 | 1880821 | 4 | 67.29 | 81039 |
| 1.20 own_images | 1000 | static | 107.446 | 10516211 | 32 | 83.00 | 96681 |
| 1.20 own_images | 1000 | interactive | 108.118 | 10532002 | 32 | 86.31 | 96681 |
| 1.20 closed_controls | 1 | static | 8.455 | 944379 | 1 | 65.57 | 78589 |
| 1.20 closed_controls | 1 | interactive | 14.805 | 1363151 | 1 | 73.83 | 155343 |
| 1.20 closed_controls | 100 | static | 29.314 | 2781552 | 7 | 78.37 | 85265 |
| 1.20 closed_controls | 100 | interactive | 46.542 | 4331301 | 7 | 78.40 | 256232 |
| 1.20 closed_controls | 1000 | static | 281.227 | 20026562 | 69 | 115.69 | 132150 |
| 1.20 closed_controls | 1000 | interactive | 455.456 | 33589493 | 69 | 158.09 | 1168017 |

All 21 ordinary operations also retain three whole-VM memory samples per
revision, with identical output hashes and the same sampling limitations.

| Runtime / workload | Largest B47 VM MiB | Largest B48 VM MiB | Output bytes |
| --- | ---: | ---: | ---: |
| 1.19 fixture invoice_012.html | 154.82 | 76.81 | 162435 |
| 1.19 fixture statement_012.html | 76.67 | 78.88 | 161286 |
| 1.19 fixture multi_page_report_012.html | 82.59 | 76.82 | 162770 |
| 1.19 fixture purchase_order.html | 90.40 | 90.41 | 124909 |
| 1.19 fixture material_requisition.html | 100.70 | 95.53 | 126178 |
| 1.19 fixture government_application_form.html | 76.88 | 78.54 | 250479 |
| 1.19 fixture stock_sticker.html | 81.16 | 75.15 | 59392 |
| 1.19 fixture trim_card.html | 90.19 | 89.60 | 163615 |
| 1.19 100-row HTML render | 90.59 | 85.87 | 166773 |
| 1.19 100-row uncompressed full-font render | 93.29 | 84.65 | 1630334 |
| 1.19 100-row PDF text extraction | 94.13 | 85.64 | 9171 |
| 1.19 100-row PDF visual text extraction | 83.25 | 83.22 | 23286 |
| 1.19 repeated text layout | 84.10 | 84.22 | 82140 |
| 1.19 long paragraph layout | 100.86 | 91.10 | 85076 |
| 1.19 five repeated PNG images | 79.17 | 75.09 | 37811 |
| 1.19 ten repeated 512px PNG images | 89.37 | 90.13 | 634859 |
| 1.19 800-word inline paragraph | 96.03 | 91.35 | 84761 |
| 1.19 160 table headings | 89.71 | 93.85 | 155020 |
| 1.19 4,000 boxes on one page | 80.15 | 76.70 | 21226 |
| 1.19 100 ordinary Text-field fill | 264.57 | 264.66 | 308620 |
| 1.19 100 ordinary Text-field flatten | 133.32 | 74.75 | 316422 |
| 1.20 fixture invoice_012.html | 82.60 | 82.58 | 162435 |
| 1.20 fixture statement_012.html | 79.25 | 77.39 | 161286 |
| 1.20 fixture multi_page_report_012.html | 85.13 | 77.47 | 162770 |
| 1.20 fixture purchase_order.html | 95.72 | 91.14 | 124909 |
| 1.20 fixture material_requisition.html | 95.46 | 80.88 | 126178 |
| 1.20 fixture government_application_form.html | 82.40 | 79.36 | 250479 |
| 1.20 fixture stock_sticker.html | 75.86 | 75.86 | 59392 |
| 1.20 fixture trim_card.html | 90.31 | 91.08 | 163615 |
| 1.20 100-row HTML render | 92.48 | 89.92 | 166773 |
| 1.20 100-row uncompressed full-font render | 102.27 | 96.65 | 1630334 |
| 1.20 100-row PDF text extraction | 90.44 | 83.91 | 9171 |
| 1.20 100-row PDF visual text extraction | 91.56 | 91.61 | 23286 |
| 1.20 repeated text layout | 84.77 | 84.79 | 82140 |
| 1.20 long paragraph layout | 90.44 | 80.79 | 85076 |
| 1.20 five repeated PNG images | 79.80 | 75.84 | 37811 |
| 1.20 ten repeated 512px PNG images | 90.12 | 90.15 | 634859 |
| 1.20 800-word inline paragraph | 96.71 | 83.93 | 84761 |
| 1.20 160 table headings | 102.23 | 86.99 | 155020 |
| 1.20 4,000 boxes on one page | 86.28 | 77.91 | 21226 |
| 1.20 100 ordinary Text-field fill | 265.34 | 265.29 | 308620 |
| 1.20 100 ordinary Text-field flatten | 148.65 | 148.67 | 316422 |

### Validation and remaining work

The popover fixture retains all visible captions, suppresses closed text and
fields, and has one page. Changed pixels are 0.1902778% with average delta
0.0003750545 under the unchanged 1% pixel threshold. Native artwork was inspected.
Both runtimes pass the full quality matrix with 1,353 unit tests, 100% coverage
and 129 browser comparisons per runtime. There are no WARN or FAIL entries.
Logs: `.quality/logs/20261011T071636Z-1189997`. The exact host command passes all 129
comparisons.

Inventory auditing retains all 17,267 original IDs. Twelve initial popover rows
move from planned to partial while broader activation/composition remains open.
Counts are 2,254 implemented, 4,712 partial, 2,183 planned, 421 printable exclusions
and 7,697 non-printing records. Full 1.1.0 and its cumulative performance audit
remain unfinished. All benchmark JSON stays ignored under .quality/performance;
bundles retain original report bytes and SHA256 digests.


## Batch 52: integration of named colors, horizontal rules and root selectors

The previously staged B49, B50 and B51 sources are integrated together. No test
modules are added. All 148 opaque CSS named colors share canonical validation;
CSS declaration grammar and ASCII normalization delegate to HtmlValidator.
Fractional atomic inline baselines and fixed dimensions retain 1/64 CSS-pixel
precision. Native horizontal rules support gray inset defaults, legacy
width/size/align/color/noshade hints, author overrides and select separators.
Ordinary block automatic horizontal margins are supported; broader intrinsic,
flex and table contexts remain partial. Inclusive root/fragment element siblings
use the existing render-scoped position cache for all ten structural positional
pseudo-classes, without synthetic element ancestors. Original hidden/style/
template siblings and virtual default-body exclusions remain covered.

The combined source passes the complete matrix on both supported runtimes:
1,369 unit tests, 100% coverage, all 131 browser comparisons and zero WARN/FAIL
entries. Logs: `.quality/logs/20261011T232034Z-2370282/`. The exact host command
passes all 131 comparisons. All 42 ordinary workload/runtime original output
pairs equal accepted B48 byte for byte, with 84 original outputs retained.
Individual color/rule/root feature scaling records remain preserved with their
original source guards. Their earlier standalone matrix results do not replace
the combined-source validation.

### Revised performance acceptance and checkpoint

On 2026-10-12 the user authorized a cumulative 10% performance allowance,
superseding the earlier 5% allowance, and requested the final performance audit
after implementation so they can decide whether its costs are acceptable.
Elapsed-time allowance applies per workload against the original pre-milestone
revision, not per batch. Reduction guardrails now allow 10%; output-size growth
remains limited to 5% and absolute timing ceilings remain unchanged. Correctness,
resource authorization and browser pixel thresholds are unchanged.

The harness can enforce elapsed-time comparisons with
`PERFORMANCE_BASELINE_REPORT`, accepting a single report or a consolidated
bundle, with optional `PERFORMANCE_BASELINE_KEY`. It requires matching runtime,
scheduler count, fixture/font/dependency hashes and workload names. It retains
the candidate report before reporting failure. Boundary checks cover exact 10%
acceptance, rejection above 10%, incompatible runtimes/inputs/workloads and bundles.

A checkpoint compared the combined B49-B51 source to the original fee6c4d source
in three balanced independent VMs per revision/runtime, using identical logical
source/application/font paths, two schedulers and CPU affinity 4,5. All original
fixtures, PNG builder, dependency lock and original font bytes are unchanged;
both applications use the same complete asset inventory. No measured code reload
or baseline revision reset occurs. Earlier original baseline reports are retained.
The median of the three independent VM medians is descriptive, not a confidence
interval. All 38 workload/runtime medians fit the subsequently authorized 10%
allowance. Positive checkpoint changes are:

| Runtime / workload | Time change |
| --- | ---: |
| 1.19 stock sticker | +1.837% |
| 1.19 800-word inline paragraph | +5.304% |
| 1.19 160 table headings | +4.541% |
| 1.20 160 table headings | +2.874% |

The earlier 5% comparison remains retained, including its inline-paragraph
finding. This checkpoint predates the B53 selector optimization and does not
complete the final full-plan performance audit. Reports are consolidated under
ignored `.quality/performance/results/1.1.0-batch-52/reports.json` and
`.quality/performance/diagnostic-captures/batch-52-output-proof/reports.json`.
Original report bytes, measured harness bytes, digests and original outputs are
retained. Final elapsed time, reductions, memory, output size, scaling and all
remaining tradeoffs must be presented to the user after the remaining plan work.

Inventory audit retains all 17,267 original IDs. The named-color definition moves
to implemented; three broader HR records move from planned to partial. Broader
color, selector, parser and layout requirements stay partial. Counts are 2,255
implemented, 4,714 partial, 2,180 planned, 421 printable exclusions and 7,697
non-printing records. The complete 1.1.0 plan remains unfinished.


## Batch 53: skip context work for impossible selector compounds

Selector matching now checks positive tag, ID, class and attribute requirements
before evaluating pseudo-classes or negations. The existing language-specific
short circuit becomes the common path. An impossible compound no longer builds
sibling-position tables or evaluates nested positional negations, while each
candidate match still consumes its normal shared CSS-work unit. Selector syntax
and semantic validation remain in their existing validators; matching does not
skip caller-input validation.

Focused existing-module coverage renders 600 elements under max_css_work=1,000.
Three impossible class/attribute/negation compounds produce the exact reference
PDF without exhausting the budget. A matching nth-child compound still returns
an actionable resource-limit diagnostic with stage, operation and named limit.
No test module or browser fixture is added because matching paint is unchanged.
The initial focused run passes 171 tests; the final common path additionally
short-circuits attribute comparison after failed tag/ID/class requirements.

The final complete quality matrix passes both supported runtimes, 1,370 unit
tests, 100% coverage and all 131 browser comparisons per runtime. All stages pass
without WARN/FAIL entries. Logs: `.quality/logs/20261011T233952Z-2465210/`.
The exact host command passes all 131 comparisons. The earlier B53 matrix was
cancelled when the user changed the performance allowance, and its partial logs
remain at `.quality/logs/20261011T233820Z-2462208/`; it is not an acceptance run.
The 10% boundary and baseline-compatibility checks pass. No performance baseline,
fixture, PDF-size allowance or browser pixel threshold is reset or enlarged.

The B52 checkpoint measurements belong to the frozen preceding source and are
not presented as fresh timings for this optimization. The final full-plan
performance audit remains scheduled after implementation, with the user deciding
whether its cumulative costs are acceptable. All benchmark reports remain
consolidated and ignored under .quality. This batch changes no inventory category
counts. Remaining parser, controls, SVG, MathML, embedding and layout work is open.


## Batch 54: required controls and requirement selectors

Boolean required attributes and initial :required/:optional matching are
implemented for the existing text-like, checkbox/radio, select and textarea
controls. Matching uses canonical applicability and attribute presence in
HtmlValidator, including disabled and readonly controls. Hidden and button input
states match neither selector. CSS identifier escapes, ASCII case, compound
negation, positional combinations and specificity use existing parser behavior.
No new input validation path, helper module or resource ceiling is introduced.

Interactive fields use the native PDF Required flag when editable. Named radio
widgets share an owner-scoped requirement if any member is required, including
intervening controls and explicit forward/outer form references. Per-element
radio attributes and CSS state remain unchanged. Unnamed radio fields remain
independent. Readonly and disabled controls retain their existing flags without
adding Required. Empty text, dropdown, checkbox and radio initial values remain
renderable; this does not enforce submission or complete constraint validity.
The flag comes from [Adobe's PDF reference](https://opensource.adobe.com/dc-acrobat-sdk-docs/pdfstandards/pdfreference1.2.pdf).
Public field-summary and diagnostics shapes are unchanged.

The pinned HTML snapshot supplies selector-required, selector-optional,
attr-input-required, attr-select-required and attr-textarea-required. Six focused
tests extend four existing test modules, with public failure diagnostics and
complete PDF equality against independent attribute-based CSS in both form modes.
Internal PDF flags are inspected through the existing validated form reader.
Focused tests pass 396 cases. Invalid selector suffixes/functions and unsupported
required attributes on button/option elements retain actionable diagnostics.

The existing control-state fixture keeps its original controls and all visible
captions while adding required/optional text, readonly/disabled requirements,
textarea/dropdown controls and an inapplicable button. Both preserved outputs
have one page and all twenty captions; hidden values stay absent. Appearance
flattening is used only for caption extraction, preserving the original PDFs
used in browser raster comparison. The final exploratory host comparison changes
0.350092% of pixels with average delta 0.000815568, below unchanged thresholds.
Optional styling explicitly excludes hidden/button input types because the
initial Chromium output showed wider optional matching on buttons. Native
applicability remains independently tested. A small author margin aligns the
new section's borders on whole-point coordinates; the broader fractional
field-border painting disagreement remains open. No writer geometry or threshold
changes are made. Initial mismatches and original pixel/PDF/text artifacts remain
under ignored .quality.

The final full quality matrix passes both supported runtimes with 1,376 unit
tests, 100% coverage, 131 browser comparisons each and zero WARN/FAIL entries.
Logs: `.quality/logs/20261012T001317Z-2618022/`. The exact host command passes all
131 comparisons. The preceding matrix at `.quality/logs/20261012T000648Z-2568961/`
failed only its 1.19 caption check, which initially extracted page text without
widget appearances. Corrected caption extraction flattens those appearances;
the full final matrix succeeds. Earlier test setup/expectation failures are
retained. Source guards cover all 276 library/test files.

Inventory audit preserves all 17,267 IDs with no omissions or duplicates. Three
required-control records move from planned to partial; broader attribute and
selector requirements remain partial while other input types and full initial
constraint validity are unfinished. Counts are 2,255 implemented, 4,717 partial,
2,177 planned, 421 printable exclusions and 7,697 non-printing records. The
user-approved cumulative 10% performance allowance remains in force. Standard
matrix performance guards pass; detailed cumulative timing, memory and scaling
review remains reserved for the final full-plan audit and the user's decision.


## Batch 55: initial disclosures and summary legends

Details and summary now preserve authored static visibility, including boolean
open presence, first direct HTML summaries, independent fallback legends, nested
disclosures and normal pagination. The closed body never creates artwork, fields
or image dependencies. Its HTML still undergoes canonical input validation.
Original DOM positions remain available to selectors even when a legend moves
ahead of preceding body content in the rendered tree. Additional summaries use
ordinary block defaults. Synthetic English fallback legends inherit typography
without receiving authored summary rules or generated content.

Named groups use case-sensitive nonempty names, permit only one initially open
member and reject nesting within the same group. These are author-conformance
diagnostics, rather than browser recovery or live exclusivity mutation. Ordinary
template fragments have independent groups. Structure and group rules live in
HtmlValidator; Style consumes its selected content. The existing budget walk
validates groups without adding another traversal to ordinary documents.

Disclosure symbols and author marker overrides reuse existing counter styles,
font fallback and list layout. Independent summary list-item layout preserves
existing unsupported nested-flex diagnostics for ordinary lists. Inside marker
advances follow pinned Chromium 151 symbolic sizing. Native uses appropriate font
triangles while Chromium uses geometric symbols, as allowed by CSS Counter Styles.
Default marker artwork therefore has small documented shape differences.
The primary source and original parity failures remain under ignored .quality.

Eight focused tests extend four existing modules, covering source positions,
marker overrides and spacing, empty/fallback content, fields, assets, executable
content, direct DOM inputs, group/template isolation and three-page pagination.
The final focused run passes 544 tests. The new browser fixture retains every
visible caption and checks that no closed body text prints. The exploratory
comparison changes 0.552525% of pixels, below unchanged thresholds. Its initial
failures exposed fallback legends skipping border/font finalization and symbolic
marker spacing; both are corrected without removing cases or enlarging the page.

The complete quality matrix passes both supported runtimes, 1,384 unit tests,
100% coverage and 132 browser comparisons per runtime, with zero WARN/FAIL
entries. Logs: `.quality/logs/20261012T015622Z-2753764/`. The exact host browser
command passes all 132 comparisons, with 1,384 unit tests excluded. Source
guards cover all 277 library/test files, with the accepted source copied under
ignored .quality for later comparisons.

The inventory audit preserves every one of the 17,267 IDs without omissions or
duplicates. Nineteen planned static-disclosure records become partial; broader
details-content, directional, shadow and parser/layout requirements remain open.
Counts are 2,255 implemented, 4,736 partial, 2,158 planned, 421 printable
exclusions and 7,697 non-printing records. Standard performance guards pass.
The cumulative 10% allowance and final full-plan audit remain in force.
The full implementation remains unfinished.


## Batch 56: editability state, unfinished

Initial code accepts the global contenteditable attribute, recognizes its empty,
true, false, plaintext-only and inherited states, and parses :read-only and
:read-write. HtmlValidator classifies supported text inputs and textareas by
their own readonly/disabled attributes, and resolves ordinary editing regions
through ancestors. That region state is cached within a render. Style delegates
matching to the validator; CssParser includes the new selector types.

Changes currently exist in lib/validators/html_validator.ex,
lib/html_to_pdf/style.ex and lib/html_to_pdf/css_parser.ex. Formatting and the
host compile with warnings-as-errors succeeded. No focused tests, browser
fixture, full quality matrix, host parity run or inventory update have been
completed for Batch 56. These changes are retained as unfinished work, not an
accepted feature. Batch 55 verification does not certify the current working
tree after these edits.

Resume by checking keyword defaults and inheritance, nested false/editable
regions, input/textarea mutability, namespaces, selector escapes and negation,
resource accounting and cache isolation. Extend the existing validator, parser,
style and public API tests, add visible browser parity coverage, then run
./scripts/quality-matrix and the exact host browser command. Update compatibility,
examples, changelog and inventory records only with verified behavior. The last
accepted source is preserved at .quality/batch-55-accepted-source/ and its hash
guard at .quality/batch-55-disclosures/candidate-source-hashes.json.

No later feature has started. The cumulative 10% performance allowance remains
authorized; the final full-plan performance audit remains pending. Work is
stopped, including further implementation and testing, at the user's request.


## Parallel continuation: first parsing slice reviewed

The user resumed the full 1.1.0 implementation and authorized parallel delegation.
The coordinator preserved the original checkout, created isolated worktrees and
published docs/html-to-pdf-1.1.0-execution-plan.md with an initial estimate of 47
remaining checkpoints. Twelve implementation tasks cover independent first slices;
accepted numbers are assigned centrally after integration.

The first parsing slice, commit 770c95f693b736277641b93a3b5474d3385e4b0a, passed
coordinator review and is fast-forwarded into the isolated integration repository.
Literal CR/CRLF normalization now precedes tokenization, reference-produced CR is
preserved, original input bytes remain the resource-accounting source, and public
failure diagnostics retain original source locations. Anchors use transparent
parent content models. Buttons accept valid supported static phrasing content;
block descendants remain rejected as the pinned standard requires. Interactive
and tabindex descendants stay forbidden, with inert-template scope boundaries.

The committed source passes the full supported-runtime matrix with 1,394 tests,
100% coverage and 133 browser comparisons per runtime, without warnings. The exact
host parity command passes all 133 comparisons. New mixed-content fixture has one
page and 0.775% changed pixels with all expected text, links and field assertions.
The initial 99.99% coverage failure was inspected and resolved by testing the
template-boundary path. Evidence is retained in the parsing lane's ignored
.quality/orchestration directory and tmp/1.1.0-results/parsing-round1.md.

This verifies the isolated parsing source, not the original checkout's unfinished
batch 56 edits. Those edits remain preserved while the controls lane completes
its checks. The original checkout will receive reviewed merged changes after
combined validation. Default inline anchors with block children still require
layout geometry and retain an explicit diagnostic. Broad parsing/anchor records
remain partial. The parser lane has begun the next bounded legacy raw-text slice.
No inventory IDs were moved by this review; no push or release occurred.


## Parallel continuation: editability reviewed and combined validation queued

The complete batch56 editability source,094608741a8fd5ecaa33cb2ef9eb0b6c8bcabb8b,
passes coordinator review. Its own full matrix passes both supported runtimes,
1,395 tests at 100% coverage and 133 browser comparisons each without warnings; exact
host parity also passes 133 comparisons. The fixture retains 24 captions and changes
0.348% of pixels. Canonical disabled state is consumed when provided, and no
separate fieldset ancestry traversal remains.

The coordinator restored all inherited56 hooks and combined the source with the
reviewed parsing slice as648e5dbe8d8a8ee5a1b7e2cb6dc0bb4642f9601c. Both complete
focused suites and fixture assertions survive the four insertion-conflict
resolutions.528 focused tests pass on the combined source. Mandatory combined
matrix and exact host validation are queued through the shared lock; source hashes
are frozen until completion. The original checkout remains preserved until this
combined acceptance. Controls now progresses to progress/meter; fieldset ancestry
and other unfinished features retain their separate owners. Evidence/task records
are in tmp/1.1.0-results/html_controls-round1.md and
 tmp/1.1.0-integration-validation-task.json. No push or release occurred.


## Parallel validation scheduling

At the user's request, the coordinator removed full-matrix serialization. The
unchanged repository quality matrix now progresses through an external scheduler
with four compute jobs on eight allocated CPU threads. Four other CPU threads
remain available for desktop work. Per-worktree locks prevent duplicate runs;
performance stages drain the compute slots and temporarily use the allocated
pool exclusively. Correctness stages use two BEAM schedulers per CPU pair;
performance stages retain their original twelve schedulers and all thresholds.

Isolated scheduler checks pass for concurrency, CPU affinity, exclusive measured
phases and lock ordering. No library, test, benchmark or matrix source was changed
by this scheduling update, so frozen integration guards remain valid. Multiple
worktrees have progressed through coverage and static analysis. Actual SVG
coverage/format and link parity failures remain with their owning agents for
resolution; they are not accepted. Early transition captures remain guardrail
evidence, while final cumulative timing will be collected on an idle host using
the fixed pre-milestone revision. No old-thread message or wake was sent.


## Validation admission fairness

A live queue audit identified measurement starvation: an older performance job
waited over11minutes while newer compute stages kept taking shared CPU access.
The external scheduler now records pending measurement intent before queuing;
new compute stages yield, existing work drains, and four compute slots resume
once measurement finishes. Existing pending jobs were enrolled without restarting
their source or validations. Stale requests are checked against process start
identity. An isolated regression check passes for priority under repeated newer
arrivals, four-job concurrency and measurement exclusion.

Observed progress confirms the correction: math operators passed its first
performance stage, viewport composition reached runtime1.20, and the combined
parsing/editability candidate passed runtime1.20performance. Remaining gates still
must pass before acceptance. Library/test/matrix/benchmark source was unchanged,
as were the eight allocated CPUs, fixed baseline and user-approved allowance.


### Eight-job and eight-agent consolidation

The user requested eight one-CPU validation jobs and eight active delegated tasks. The external scheduler now has eight singleton slots on CPUs 0,1,2,3,6,7,8,9 with +S 1:1. Previously loaded paired jobs drain by PID/start identity before new jobs enter. Performance keeps exclusive access to the same pool, original +S 12:12 settings and unchanged source/thresholds. An isolated scheduler check verified eight simultaneous jobs with disjoint affinity, benchmark priority, and no measurement overlap.

Seven feature owners continue; five smaller lanes provide durable handoffs to those owners. A dedicated validation role fills the eighth slot. Preserved patches with interrupted host or runtime checks remain incomplete. Full matrices that already passed are retained as evidence.

Combined parser/editability checkpoint 648e5dbe8d8a8ee5a1b7e2cb6dc0bb4642f9601c passed Elixir 1.19 and 1.20 matrices, each with 1,405 unit/coverage tests, 100% coverage and 134 parity tests. Exact host parity passed 134 tests. No matrix FAIL/WARN entries. All 292 source hashes matched. Thirteen changed source/test/fixture paths were copied into the original checkout after checking its initial hashes, with a backup and copy report in tmp/1.1.0-integration-copy-result.json. No user Git commit, push or release was performed. The final cumulative 10% audit is still pending.

Earlier in-place external runner replacements caused command fragments such as `ogress: command not found` in transitional shell output. These are orchestration defects, not project or third-party warnings. Future runner updates must replace the file atomically and preserve active scripts; do not rerun successful project gates solely for this stderr.


### Verified lanes and second integration wave

Inline images `9def7148` and embedded authorization `0a2f148` passed both runtime matrices and exact host parity, with 100% coverage and no final WARN/FAIL entries. The preserved metadata `452cb51b`, responsive selection `cd51822`, operator semantics `b29f30b` and viewport groups `a15c3d8` also passed their required gates. The viewport includes the nested private-field validation fix and regression evidence. These commits implement bounded scopes; broader feature obligations remain open.

Root combined `648e5db` with inline images, metadata, operators and embedded authorization as isolated candidate `b26f42fa7d9ad9057724b79ce04782ba37f6d8d5`. The merge retains both complete test and fixture assertion blocks. The combined focused suite passed 852 tests with warnings as errors. The candidate is frozen under 319 source hashes while matrix `20261012T035759Z-3944170` and exact host parity run under a dedicated validator. The original checkout remains at verified `648e5db` until combined acceptance.

MathML M1 remains unaccepted. Its focused tests and real token fixtures pass, while the unmodified row comparison retains a documented 1.28935% disagreement with Chromium. Source-derived Core reference adjustments and a direct default-token fixture are recorded separately. Full gates exposed a compiler warning, uncovered branches and Dialyzer findings. The owner corrected those and continued the pinned-source text-node and whitespace review before another final run. Cases and thresholds remain unchanged. SVG native loading also remains under its owner; the actual NIF upgrade warning is identified in its logs.

### Legacy parser checkpoint and inventory evidence

Legacy raw-text checkpoint `682f6efe5978e4917552ee174479f6326d4d98b0` passed both runtime matrices with 1,406 unit tests, 100% coverage and 134 parity tests each. Exact host parity passed 134 tests, with no final WARN/FAIL entries. Xmp, noembed and plaintext parsing now preserve literal content, contexts, source boundaries and original diagnostics. The noembed behavior after author display overrides follows Chromium's no-box behavior; its difference from the pinned default CSS is documented. H10 remains open.

The next parser round implements retained processing instructions, addressable earlier-node trees and template replacement. Composition continues child rendering independently. Processing instructions must preserve original text-node boundaries and must not affect element sibling indexing, paint or resource loading.

Inventory evidence now reflects verified original-checkout support for twelve contenteditable, anchor and button records. Broad rows stay partial; pending integration work was not promoted. Regenerated category documents passed the source audit, accounting for all 17,267 IDs exactly once, with no missing or duplicate records. Classification accounting does not establish renderer completion.

### Inline responsive images and next batches

Image checkpoint `6dff76a1450f643d545982529e6af6233d9afb0c` combines accepted responsive selection, parser/editability and inline image layout. Default inline pictures preserve density, natural and selected-source dimensions, zero sizes, fitting, links, wrapping and pagination. Both matrices passed 1,434 unit tests at 100% coverage and 136 parity tests each; exact host parity passed 136 tests. All 462 guards match, and no final WARN/FAIL entries remain. The five-page fixture retains all labels, 20 image draws, three URI targets and exact annotation counts, with a maximum changed-pixel ratio of 0.872685%. The print viewport and codec reference caveats remain documented.

That immutable checkpoint awaits the next combined integration wave while its agent works on validator-owned table occupancy, empty rows and span rules. The composition agent has imported accepted viewport groups and is testing native child placement, local resources, isolation, clipping and transformed image link rectangles. Interactive child widgets currently receive an explicit geometry diagnostic. Full associations, parser replacement, child dispatch, MathML, SVG and final cumulative performance acceptance remain unfinished.


### Second combined wave accepted

Candidate `b26f42fa7d9ad9057724b79ce04782ba37f6d8d5` passed both supported runtimes with 1,464 unit tests, 100% coverage and 136 parity cases each. Exact host parity passed 136 cases. Performance and Dialyzer passed, with no warnings. All 319 frozen hashes matched and the isolated tree was clean. Root checked the original checkout hashes and copied 28 changed source/test/script paths, preserving backups and updating its manifest. The original checkout now matches this verified integration.

The third combined wave, `6e80f25303b914cecddc18a27239b301f2c4bc2d`, combines responsive inline pictures and legacy parsing. Its focused suite passed 793 tests, and both runtime matrices passed 1,490 unit tests at 100% coverage and 138 parity cases. Exact host parity passed 138 cases, with no warnings. Six byte-identical overlapping test duplicates were removed while preserving distinct cases; the Limits count and picture API documentation were corrected. Root copied its 18 changed paths after verifying all 473 snapshot hashes and 325 original runtime/input hashes. Backups and the copy report are retained in `tmp/1.1.0-wave3-integration-copy-result.json`.

H8 gauges `d007ee15` and H11 base/link preparation `a40572b3` have also passed their individual mandatory gates. They await later combined integration. Controls now integrates the fieldset handoff and proceeds to initial dimensions/selection. Metadata now implements final scoped PDF destinations. The composition agent has received the accepted H11 child URL contract, and will complete its bounded viewport checkpoint before required scrollbar geometry. These remain implementation checkpoints, not whole-milestone acceptance.

### Fourth combined wave and continuing feature work

Visible table formation `91ba289e3f85d74a84eafe908d9fe43128794b3e` passed both runtime matrices with 1,443 unit tests, 100% coverage and 137 parity cases each, plus 137 host parity cases. All 465 source hashes match and no warnings remain. The validator now owns visible rows, columns, spans, empty rows and occupancy. Logical implied rows, model diagnostics and header associations remain under the next table round.

Processing-instruction retention `a5a7735ceff8c25795a2997f0301e73d68ef8b86` passed both matrices with 1,417 unit tests, 100% coverage and 135 parity cases each, plus 135 host parity cases. All 317 source hashes match. Matrix warnings are zero; a host preflight PLT rebuild emitted 31 inspected third-party OTP xmerl missing-spec warnings. PIs retain validated literal data and original source locations without painting, fetching resources, creating fields or changing element sibling selection. Live parser addresses and parser-time template replacement remain under the next parser round.

Wave 4 combines these two accepted commits with gauges and base URLs in the isolated snapshot. The original checkout remains at verified wave 3 until combined gates pass. Seven feature owners and one integration owner are active, with eight singleton CPU build slots and exclusive performance measurement. Bounded child rendering is frozen for its required gates after all three new Chromium fixtures passed. Automatic scrollbars remain a required follow-up. MathML, SVG, destinations, controls and the final cumulative 10% performance audit remain open.

SVG processing and byte-backend checkpoint `79293f63bab82c4b03edab7f71ff7c9472b1736d` passed both matrices with 1,403 unit tests, 100% coverage and 133 parity cases each, plus 133 host parity cases. Its 300-file guard matches the immutable commit; no matrix warnings remain. Native loading and font-import failures were corrected. The adapter fixture passes at 0.96875% changed pixels with unchanged thresholds; its original 1x raster disagreement remains documented. Inline SVG rendering remains blocked until host cascade, viewport and scene composition work is implemented. A new SVG round continues that work while the accepted adapter waits for wave 5 integration.

MathML token/row checkpoint `2383f663f3516e14cbb9cacdb23663e98c3ea0db` passed both matrices with 1,399 unit tests, 100% coverage and 135 parity cases each. Performance and Dialyzer passed without warnings. Exact host parity passed 135 after one preserved Chromium timeout in an existing report fixture; the retry changed no source, tests or thresholds. The direct default-token fixture passes at 0.363426%; the approved source-derived row reference passes at 0.388889%. The unmodified row disagreement remains preserved at 1.289352%, rather than being treated as passing evidence. M1 includes original ActualText and selected-GID ligature fixes; independently painted HTML children still diagnose pending composition. Root checked the preserved source patch and both new fixture hashes against the commit. Operator and HTML-child work continues in a new MathML round; broader MathML remains unfinished. The immutable M1 commit waits for wave 5.

Bounded child viewport checkpoint `77c7b538e706a7a3c174a9ace78dfd899e93ee5b` passed both matrices with 1,484 unit tests, 100% coverage and 141 parity cases each, plus 141 host parity cases. No final warnings remain. Root verified all 477 source guard hashes and each runtime's 107 performance source/input hashes against the commit. Its three actual iframe fixtures pass below 1%, preserving local resources, isolated document styles and static controls. The commit includes accepted viewport, legacy parser and base-URL changes and resolves transformed image border-box link regions independently from image fitting. It waits for wave 5 integration. The same owner continues required automatic-scrollbar/client-area geometry; transformed interactive widgets, arbitrary affine effects, fragment destinations, object/embed/frames selection and HTML raster composition remain unfinished.

### Fourth combined wave accepted and copied

Wave 4 `27bbcc142ea817792a16b9d56c5ea7eb5069315a` passed both supported runtimes with 1,544 unit tests, 100% total/module coverage, performance, Dialyzer and 143 parity cases each. Exact host parity passed 143. All 20 matrix events and inspected mandatory logs have zero warnings. A preflight PLT rebuild's 31 upstream OTP xmerl missing-spec warnings remain identified in its report. All 483 frozen hashes and each runtime's 112 performance input/library hashes match the commit. Root checked the original manifest, retained backups and copied all 31 reviewed paths. The copy report is `tmp/1.1.0-wave4-integration-copy-result.json`; coordinator documentation and user Git history are preserved.

Wave 5 now combines only accepted SVG1, MathML M1 and bounded C6 in the isolated snapshot. Seven feature owners plus the integration owner remain active. The new C1 addressing checkpoint is unaccepted: its first full run passed coverage but failed the unchanged 100-row reductions guard on both runtimes. The parser owner is profiling tree/source overhead before another freeze. Logical table/header work is frozen for gates, destinations are rerunning corrected coverage gates, and controls, operators, SVG composition and scrollbar work continue. Required behavior and the final cumulative performance audit remain open.

Scoped root-document destinations `70381065d4a7e0b567fb89cf57188010c5178f01` passed both matrices with 1,488 unit tests, 100% coverage and 138 parity cases each, plus 138 host parity cases. No final warnings remain. Root verified all 474 source guard hashes and each runtime's 108 performance hashes against the commit. Links resolve from final pagination and serialize actual PDF page references; the shared first-ID lookup uses logical document/tree scope without granting resource permissions. Its two-page fixture passes at 0.4892677%, while the original base fixture remains unchanged. The integration owner has been asked to add this accepted delta to wave 5 before freeze. Child/group destinations and stylesheet/print selection continue in a new metadata round; the table and parser owners received the accepted shared helper contract.

Logical table/header checkpoint `3fb59a94607cfa02d7ef18e9e0653ab9e15dbfbc` passed both matrices with 1,462 unit tests, 100% coverage and 138 parity cases each, plus 138 host parity cases. Root verified its 467 source hashes and each runtime's 103 performance hashes against the commit. Final gates have no warnings; 31 upstream OTP missing-spec warnings from the initial host PLT rebuild remain identified. The validator preserves logical implied rows and explicit/automatic header associations independently of CSS row-group clipping. It prepares spans once and preserves source metadata, authored attributes and model diagnostics without adding PDF accessibility structures. This accepted delta has been requested as a fifth wave 5 input before freeze, with the temporary private ID lookup replaced by the accepted shared helper. The same table owner continues block captions and presentation; broader table formatting and fragmentation remain required.


## Recovery after capacity interruptions

The model-capacity failures in Wave 5 integration, parser round 4 and metadata round 3 are orchestration failures. Source and independent validation processes were preserved. The original code-and-tests workflow remains in force.

Root verified frozen1629 source: all512 hashes match, both runtime matrices passed1642 unit tests at100% and154 parity, performance and Dialyzer; exact host154 passed and guarded wrapper exit0. GitHub CI still lacked Cargo/Rustc on both configured bare images. The reviewed four-line provisioning patch bedad153 was applied in isolated snapshot as d20a6165e33a9949923106c747dfce9923f0858c only after wrapper exit. Fresh mandatory guarded validation is active, session34243, driver .quality/orchestration/integration-wave5-ci-driver.py. No snapshot/source edits during this freeze. Root original remains verified Wave4 27bbcc1. No push/release.

Math M2 input staged separately as f36a726 with operator13 and integration10 focused checks passing; this is unaccepted combined staging. Controls staging remains active and must end with a bounded durable handoff. Helpers completed read-only review and Rust provisioning smokes on both bare images.

Current runtime limit is four agents including root. Existing in-flight agents are preserved; recovery rounds are queued for free slots, with metadata child validation and parser C1 accounting highest priority. Eight singleton CPU compute jobs and exclusive unchanged performance scheduling remain configured.

Metadata child490-file source remains frozen under its existing run. Resume owner must inspect terminal result without duplicate matrix, finish acceptance only on hashes/log evidence, then address H11 actual client CSS viewport before Style/font/resource selection. The accepted77 implementation styles before viewport is known; no parent/A4 approximation or wrong-context fetch is acceptable. Same issued context and budgets must survive any context-sensitive re-style.

Parser second frozen matrix063758Z-1358062 and exact host135 passed. C1 is still unaccepted pending bounded failure-path UTF8 diagnostic source-scan accounting. Preserve performance optimization and all first-failure artifacts; no threshold/baseline/input changes.

Layout owner reported an unexpected host SIGTERM after its1468 unit100%/139 parity matrix passed. Root performed no cancellation or kill. Owner is retrying only unfinished exact host on unchanged471-file source.


Layout round 3 is terminal completed at clean72033d893d0a809a6e5d88877200dc4df56f0c13. Caption/source-lookup gates1468 unit100%/139parity each and host139,471 hashes; zero-column geometry probe failed as expected and was removed after preserving direct evidence. No library changes from that probe remain. Metadata later continuation handed back clean9c2b14 with490 unchanged hashes, no H11 source batch or owned job left. The exact6dff PrintMedia copy is ignored review evidence only. New C6/H11 rounds must retain these immutable contracts; do not wake completed old tasks.


Root packaging correction update: d20a616 both runtime matrices PASS1642unit100%/154parity/performance/Dialyzer, all19actuallogs clean and20input/98library hashes/runtime match12schedulers. Initial host stopped at broken localmiseCargo shim; unchanged exact host with installedRust PATH passed154,source512unchanged. Separate fresh staging compile exposed actual NativeBytes on_load bad_name before application ebin registration. Five-line Mix compiler fix d961ee6 proves cold native load and76bytePNG without warnings. Applied to isolated snapshot as38333b9235899696e86052f80915d0c6b37bfe3c afterhostexit. New guarded mandatory run active session85665,source512frozen; no duplicate matrix or active-source mutations. Original checkout still accepted27bbcc1.

Combined next staging at integration_staging/wave6-v2/combined includes controlsadf9917,M26dc4da9,captions5cb2e2a plus proven compilerfixd961ee6. Caption full shared metadata module and once-per-scope table authority retained; fiveconflicts preserved/resolved,120focusedpass. Child9c import18overlaps from bundledC6 were captured andabortedbeforefurtherwork. No combined acceptance or fullmatrixyet.


## Verified integration Wave 5

The combined checkpoint adds bounded SVG assets, MathML token/row rendering,
local embedded documents, root fragment destinations and logical table/header
metadata. Permanent Rust sources and pinned CI provisioning support the SVG
backend. Fresh compilation registers the application before loading its private
native binding.

Both Elixir 1.19 and 1.20 passed 1,642 unit tests at 100% coverage, unchanged
performance gates, Dialyzer and 154 browser comparisons. Exact host parity
passed 154. The final source guard matched all 512 tracked files and final
logs had no warnings. The original checkout's permanent runtime, test and
build files match this tested source.

Wave 6 combines later operators, controls, captions, child destinations and
parser identity work in isolation. Pending source is not included in this
checkpoint. Full feature completion, documentation/inventory reconciliation
and the final cumulative performance audit remain required.
