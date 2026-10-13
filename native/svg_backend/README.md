# SVG byte backend contract

This adapter is the SVG-1 foundation, not the inline SVG renderer. The existing
restricted `SvgRasterizer.rasterize/3` API remains unchanged. The new internal
`SvgBackend.rasterize/4` validates one complete XML document before native work.
It never extracts or rasterizes an HTML substring.

## Reproducible build

`Cargo.toml` pins resvg/usvg 0.47.0, tiny-skia 0.12.0 and rustler 0.33.0.
`Cargo.lock` retains the dependency graph used by the installed resvg NIF.
Mix builds this tracked crate with `cargo build --release --locked`, using a
separate target directory for each Mix build. Its private `bindings.ex` is compiled
alongside the shared library into `priv/native`, not application `ebin`. The
adapter loads this private native artifact through the VM code loader. Elixir
coverage includes the application adapter/validators; integration tests exercise
the actual native bindings and Rust renderer. No coverage exclusions or lowered
threshold are used. Dialyxir analyzes both application ebin and the private native
binding directory. The quality image supplies Rust
1.92.0. Each matrix run uses its own Docker tag so concurrent worktrees cannot
replace the native toolchain image between stages. Host builds require Cargo and a compatible Rust toolchain on PATH.
Cargo registry sources remain checksum-verified; no patches in `deps/` are used.
The adapter source is distributed under this repository's MIT license. Upstream
crate licenses remain those recorded in their Cargo packages, including resvg's
MIT/Apache-2.0 dual license.

## Inputs and ownership

`SvgValidator.validate_processing/2` accepts `%{mode: :inline | :image |
:reference, parent_mode: mode, fonts: descriptors, resources: descriptors}`.
The default parent is `:inline`; an image parent restricts every descendant
context. `:image` forbids external resource dependencies. `:reference` accepts
only explicitly supplied resource identifiers. This is a static application
policy, not a claim to implement every SVG processing-mode feature.

The composition resource layer owns authorization, immutable roots and resolution
bases. It supplies already authorized bytes. This adapter performs format,
structure and resource validation, never file loading, authorization or fetching.
Resource keys are opaque `svg-resource:NAME` identifiers. Values contain
`%{data: png_bytes, media_type: "image/png"}`. Font descriptors contain
`%{data: true_type_bytes}`. Additional descriptor metadata is retained by the
owning resource layer; the native boundary receives only identifier/byte maps
and font binaries. PNG validation uses the existing decoder and image budget;
font validation uses the host font parser, source fingerprints and budget.

The Rust resolver returns only PNG entries from that map. Data-URL decoding is
disabled, `resources_dir` is unset, no system fonts are loaded, and the font
database contains only `load_font_source(Source::Binary(...))` inputs. Pinned usvg font selectors query
that database and do not perform I/O. The first supplied font establishes generic
family defaults; complete host family/weight/fallback selection remains SVG-5.
Caller context options cannot install backend resolvers or directories.

Authored source/node/path/filter/reference work and expanded reference subtrees
charge the existing reentrant render scope. Output and native resource pixels share
one cumulative SVG raster-work counter. Each expanded PNG occurrence charges
its native decode pixels and RGBA bytes, including repeated use instances. Font parsing work charges repeated byte
occurrences separately from deduplicated host font source fingerprints. Cycles, duplicate IDs, missing
rendering reference targets and expansion exhaustion fail before native access.
Repeated siblings remain legal. Expanded path/point bytes are charged for every
reused node, so repeated `use` instances cannot bypass the geometry ceiling. Per-source ceilings remain independently checked. Text codepoints charge
expansion work before shaping, including text in reused subtrees.
Cache integration must re-charge these occurrence costs; this batch adds no cache.

## Capability gates and remaining work

`SvgBackendValidator` accepts only the bounded backend element profile named in
its capability check. Inline mode always fails until host cascade and viewport
composition exist. `foreignObject` fails specifically because pinned usvg has
no conversion branch or element registry entry. HTML scene insertion must happen
at the original scene position before ancestor effects. Painting HTML after a
flattened SVG would not preserve the required behavior.

Author clipPath, stylesheet scenes, patterns, masks, filters and markers are gated in this adapter until intermediate
allocation and composition can be prepared and bounded. Nested SVG image resources
are gated until recursive authorization and expansion budgets are integrated.
Text requires actual font bytes and coverage for every codepoint; an empty font
database is never accepted as successful text rendering. This is a byte-font
adapter test, not complete SVG text/layout or logical-text/PDF link integration.
General SVG semantics, host style/viewport layout, author rendering hints, full
affine transforms, PDF logical content, effects, native HTML and cache correctness
remain SVG-2 through SVG-8. Existing assets retain their restricted converter;
its broader replacement awaits these capability and allocation guarantees.

The future scene should carry measured layout/ink extents, baseline, artwork,
logical text and links. It can consume writer `drawing_group` for positive axis
scales/translations. Rotation, skew, reflection and ancestor SVG effects require
later composition support. `glyph_group`/ActualText remains text-specific.

Pinned implementation evidence:

- [usvg image resolvers](https://github.com/linebender/resvg/blob/v0.47.0/crates/usvg/src/parser/image.rs)
- [usvg font resolvers](https://github.com/linebender/resvg/blob/v0.47.0/crates/usvg/src/text/mod.rs)
- [usvg converter](https://github.com/linebender/resvg/blob/v0.47.0/crates/usvg/src/parser/converter.rs)
- [usvg element registry](https://github.com/linebender/resvg/blob/v0.47.0/crates/usvg/src/parser/svgtree/names.rs)
- [SVG processing modes](https://www.w3.org/TR/SVG/conform.html#processing-modes)

Chromium can paint foreignObject and host-styled inline SVG; this adapter's
explicit diagnostics preserve the whole-feature deferral instead of claiming
backend omissions meet those standard requirements.

The adapter parity fixture embeds the same DejaVu Sans bytes in Chromium's SVG
image and supplies them directly to the native font database. It authors explicit
geometricPrecision/optimizeLegibility hints and renders native artwork at 480×160
pixels into a 240×80-point PDF image. One-pixel-per-point output retained a measured
1.016% changed-pixel disagreement from raster glyph edges; the 2× image passes the
unchanged 1% threshold. Default optimizeSpeed output and complete vector/logical
text parity remain follow-up work, not closed SVG requirements.

The first slice uses a conservative cumulative temporary-surface allowance of
75 × output pixels × expanded node/text work. The fixed factor derives from the
pinned renderer's actual max_bbox of five canvas widths/heights (25× area), plus
up to three layer/clip surfaces per expanded work item. The nearby upstream
comment says four; the code uses five. The tunable allowance itself lives in
Limits.max_aggregate_svg_surface_pixels. This can reject large but otherwise
valid scenes; exact scene/intermediate accounting and author clipPath/CSS support
remain required follow-up work. No caller option disables this accounting.

The native adapter checks fontdb's import result for every supplied byte source.
A host-readable glyph/metrics font lacking an importable named face returns a
conversion diagnostic instead of silently omitting text. Backend import failure
is an execution postcondition; caller byte/structure/glyph validation stays in
FontValidator and SvgBackendValidator. Complete font selection, outline/text
semantics and logical drawings still belong to SVG-5.

## Selected-font text paint servers

Inline text fills accept the same validated local gradients and patterns as
geometry. Axial/radial shading patterns and colored tiles fill genuine selected
CID glyphs directly. Whole-text object bounding boxes and user-space coordinates
come from the prepared usvg scene. Each layout span allocates its paint resource
once, before its original glyph clusters paint. Original FontFile2 bytes, glyph
IDs, source Unicode and once-per-cluster ActualText remain intact.

Pattern matrices follow the default user space of the containing PDF stream.
Transparency forms, masks and tile streams establish their own paint origin;
full scene transforms continue to describe clipping and selection geometry.
Existing gradient/path/font work quotas apply before PDF allocation. Stroke
text remains the separate glyph-program implementation lane.

The batch passes79 focused tests. The original gradient and both selected-text
Chromium fixtures pass. The original patterns/markers fixture remains red on
this lane at5.642% changed pixels and1.294% average delta. The new original text
paint-server fixture is red at2.475% changed pixels and0.362% average delta
against unchanged1% gates. Their sources, PDFs, rasters and earlier failures are
preserved. Paint-server visual parity remains open.

## Native patterns and markers

Inline scenes admit local pattern definitions and inherited marker-start,
marker-mid and marker-end properties through SvgEffectsValidator. Pinned usvg
resolves pattern units, inherited definitions, marker units and orientations.
The PDF writer paints patterns with colored tiling resources and marker artwork
with ordinary transformed vector paths. Pattern child path and gradient work
charges the same native work counters as outer artwork. Raster preparation counts
each resolved tile's exact pinned round-to-u32 dimensions before tile allocation
and reserves them against max_aggregate_svg_surface_pixels.

The original inline_svg_patterns_markers Chromium fixture retains the existing
1% changed-pixel and average-delta thresholds. Its first comparison is red,
with 5.642% changed pixels and 1.294% average delta. Native rotated tiling-cell
antialias seams remain to be resolved. The focused semantic/resource/render
tests pass, but this does not close S4 parity or the S6/S7 implementation scopes.

Inline masks now use native PDF transparency groups and Alpha/Luminosity soft
masks. Mask coordinate units and regions resolve through the validated usvg
scene. Colored luminance masks convert authored sRGB colors using SVG's fixed
0.2126/0.7152/0.0722 weights before PDF luminosity conversion. Group effects apply
once to the composed artwork, so overlapping children and genuine selected-font
text remain inside the mask. Mask children, linked masks and their paint servers
charge path, gradient and raster intermediate budgets before execution. Raster
allocation bounds use each resolved layer's transformed extent, pinned antialias
padding and the pinned 25-canvas-area cap, rather than a full-canvas reservation
for every small mask.

The original inline_svg_masks fixture retains its 1% thresholds and is currently
red at 1.449% changed pixels, with 0.206% average delta. The changed pixels occur
mainly at mask-region and artwork edges. Focused mask/pattern/reuse/text tests
pass44/44. This provides actual mask behavior, while mask parity, filters and
native foreignObject composition remain open.

## Native HTML Form slots

Inline foreignObject preparation retains its original host-styled HTML subtree,
source identities, namespace, selected faces and issued document resource scope.
An execution projection replaces each viewport with a validator-issued local
rectangle identifier. Native vector export intercepts only those identifiers and
emits an empty Form slot at the original scene position under the same affine
transforms, clips, opacity and vector masks. Slot metadata supplies the actual
resolved viewport size and full root-PDF affine placement. Hidden geometry does
not issue a painted slot.

SvgFormComposition lays out the original styled children through the bounded
native HTML viewport and uses PdfWriter.appearance_objects to build real drawing
Forms, then replaces the slots through the shared incremental PDF writer. This
preserves real selected-font text and backgrounds without a bitmap or invisible
text surrogate. HTML navigation hrefs use the existing URL authority rather than
SVG resource-fetch authorization. Namespace declarations on HTML accept only the
fixed XHTML namespace, and serialization respects HTML's implicit namespace on
ordinary descendants. Annotation-only link_region boxes carry genuine URI link
bounds through the writer's existing clipping and page-placement contracts.

Focused SVG/slot/writer/selected-text tests pass155/155 on published base
1cbc7ab, with one preexisting excluded test.
The original foreignObject fixture now renders, but its fixed 1% Chromium check
is red at 1.638% changed pixels and 0.196% average delta. Both its earlier namespace
failure and its rendered comparison remain saved. Fragment target transport,
exact HTML ink accounting and arbitrary clip-path/rotated annotation polygons
remain open. Filters must receive the actual composed HTML effect input before
being enabled; an empty slot cannot stand in for that input.

## Static filter graphs

SvgFilterValidator owns local graph inputs and result names, operation nesting,
units, finite numeric parameters, matrix/kernel dimensions, channel transfer
functions and lighting inputs before usvg can normalize malformed input. Inline
filter references render their actual resolved group once through pinned resvg,
then embed the resulting RGB image and alpha mask at the original local paint
position. Ancestor transforms and clips remain in the PDF paint sequence, while
unaffected siblings retain native vector and selected-CID text output. Required
static operation families have focused rendering coverage. feImage resource
inputs and filtered HTML surfaces remain unfinished and cannot be evaluated as
an empty HTML Form slot. filterRes and kernelUnitLength remain diagnosed.

The native effect validator reserves cumulative temporary canvas/result pixels
and parameter-dependent convolution, morphology, turbulence, blur and lighting
work before allocation. max_svg_filter_work, max_svg_filter_kernel_values and
max_svg_filter_octaves belong to Limits alongside the shared cumulative surface
ceiling. Graph work includes nested masks, patterns and filter groups.

Effect-image replacement text uses only the original issued selected visible
clusters belonging to that effect group. Empty rendered effects and hidden or
viewport-excluded text emit no replacement. PDF 1.7 section10.8.3 permits Span
ActualText for graphics representing characters. Standard Text extraction now
handles actual Image Do replacement geometry, reports font_resource nil and an
image_resource, and keeps text/font validation for genuine text operands.
Effect-image spans describe the image region, never individual glyph positions.
Original selected glyph plans stay intact for selection and link integration.

Focused filters, masks, patterns, foreignObject, Text and SVG authority checks
pass161/161 with one preexisting exclusion. The new original inline_svg_filters
fixture passes its unchanged 1% Chromium gates at0.638% changed pixels and0.099%
average delta. The older pattern/mask/foreignObject red cases remain preserved.
This batch does not complete real HTML effect inputs or filter link geometry.
## Initial HTML form-value helper

The same locked Cargo build creates `native_form_values`, a byte-only disposable
helper copied atomically beside the NIF. It compiles original patterns before
adding HTML's full-value anchors, with ECMAScript Unicode-v sets, string
properties, backreferences and lookarounds. Matching uses UTF-16 code units
with Unicode code-point semantics. Invalid authored patterns are ignored.
URL values use `url` 2.5.8's WHATWG parser without a base. Neither operation
loads a URL, file, script, font or other resource.

The vendored regress 0.12.0 archive and original source SHA256 values are recorded
in `vendor/regress/INSTRUMENTATION.json`, with upstream licenses preserved.
Instrumentation charges compiler/VM entries, loop backedges, character scans,
backreference comparisons and set-string comparisons. Recursive calls consume
the configured depth bound. A global allocator charges every actual allocation
request, including Vec growth, Box/String/map allocations, Unicode string-set
copies and optimizer output, before calling the system allocator. Accounting is
cumulative reserved bytes, so reallocations charge their full new reservation.
The native parser's previous hard-coded nesting ceiling is replaced by Limits.
The 65,535 capture-group and loop bounds are fixed u16 bytecode representation
invariants. Exceeding them produces a resource diagnostic, never an invalid
pattern or invented mismatch.

A disposable process permits allocation exhaustion to terminate that helper
without unwinding from a global allocator or terminating the rendering VM.
Exit statuses report allocation, compilation, execution, depth and elapsed-time
exhaustion separately. The watchdog terminates the helper at the supplied time
limit. Validators prepare UTF-8/shape/size input, supply every configurable
ceiling from Limits and charge actual successful work to the existing reentrant
render budget. The execution adapter only handles framed byte transport.

Relevant primary definitions are the [HTML pattern attribute](https://html.spec.whatwg.org/multipage/input.html#the-pattern-attribute),
[regress 0.12.0](https://docs.rs/regress/0.12.0/regress/), and
[url 2.5.8](https://docs.rs/url/2.5.8/url/).

## Internal HTML filter inputs

Filtered foreignObject content uses the original issued HTML drawing group and
this library's own PdfWriter painting PDF. Pinned hayro 0.8.0 interprets only
those bytes with embedded fonts, disabled annotations, and rejecting font/CMap
fallback resolvers. No external PDF rendering/embedding API or filesystem
resolver is introduced. This dependency requires Rust 1.92 or newer.

Native preparation reserves actual source image dimensions, canvas/layer copies,
transformed path coverage work and original embedded outline glyph plans before
raster allocation. The pinned canvas uses u16 physical dimensions and the pinned
interpreter has a 50-level interpretation stack; validators diagnose these
representation bounds before execution. Caller-controlled resource ceilings
remain in Limits. Ordinary unfiltered HTML remains a genuine PDF Form.

Actual effect image marked content binds the original selected Unicode/GIDs,
matrices and ink bounds to the painted group. Standard extraction reports the
image region, while original glyph and link geometry stays attached to its
issued scene placement. Rectangular ancestor and HTML viewport clips select
actual glyphs before replacement text projection. More complex mask/clip ink
selection, selected-cluster cases diagnosed by the validator, and transformed
interactive HTML transport remain open.

The new original inline_svg_html_filters fixture keeps the 1% gates. Its initial
comparison is red at 3.516% changed pixels and .387% average delta. A 300-ppi
sampling experiment remained red at 3.708% and .446%; the existing bounded
sampling default is retained. Original inputs, references and both red artifacts
are preserved. This implements real HTML SourceGraphic and logical input but
does not close S6/S7 parity or the remaining static graph obligations.

## Local filter image inputs and shared paint servers

Local feImage inputs use their original same-scene vector targets, including
transforms, gradients, bounded patterns and nested static effects. Input-root
stroke, gradient and intermediate surface work is counted before rendering.
Independent selected text inputs now use issued glyph plans as described below.
HTML and external byte image inputs remain diagnosed until their independent
selected-input plans and resource transport are added.

The pinned usvg source has a shared-filter defect: image paint-server resolution
used Arc::get_mut and skipped repeated filter definitions. This repository
vendors checksum-verified usvg0.47.0 with the corrections recorded in its provenance manifest. Filter instances
can clone the same original filter ID, while general node IDs remain
non-cloneable. Arc::make_mut resolves each image input's actual gradient space.
The original crate checksum and before/after per-file provenance are tracked in
vendor/usvg-0.47.0/native-patch-provenance.json; MIT/Apache licenses are retained.

The new original inline_svg_filter_images comparison improves from4.661%changed
and.838%average to1.430% and.243% after the shared paint-server correction. The
fixed1% gates remain red on edges/antialiasing. Both original artifacts remain
intact. This does not close feImage or S7 parity.


## Independently selected filter text inputs

Local feImage references can paint independently issued SVG text. Its original
selected font bytes, glyph IDs and Unicode are prepared from that actual input
root, and a bounded source graph binds live input text to the real image Do.
Flood or constant output cannot claim unrelated SourceGraphic text. Hidden,
transparent and fully clipped selected inputs emit no logical replacement.

The pinned Chromium152 reference maps viewport-dependent referenced paint from
the nearest SVG viewport into the primitive subregion. Its layout_svg_text.cc
marks actual text layout viewport-dependent even with absolute font sizes.
The vendored parser therefore applies that actual input-group transform while
preserving authored font sizes and selected glyphs. The source proof is
svg_fe_image.cc at tag152.0.7977.82, linked in vendor provenance. This change
covers referenced text and its visible containing groups; broader percentage
shape and nested viewport dependence remain to be implemented.

The original inline_svg_filter_text_inputs case passes its unchanged1% gates.
Exact nonlinear output-ink selection, per-cluster filter displacement and links,
HTML/resource feImage inputs, filterRes and kernelUnitLength remain open.
