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
1.90.0. Each matrix run uses its own Docker tag so concurrent worktrees cannot
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
