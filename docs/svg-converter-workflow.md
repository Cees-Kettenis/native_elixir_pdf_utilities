# SVG conversion

The HTML renderer extracts SVG source and uses the existing `Resvg` dependency
to produce PNG artwork. That PNG follows the same decoding, layout and PDF image
embedding path as other images. SVG painting is not implemented by this library.

The Elixir binding is pinned to `Cees-Kettenis/resvg_nif` commit
`850f23d690bd8cce9276d35e2ab43de6f487d4d5`. Its `0.6.0-pdf.2` release supplies
checksum-verified precompiled NIFs. Both Rust crates are pinned to
`Cees-Kettenis/resvg` commit `fb9ddf1aacbffc6c1b5a5f0f50a6c079e08fb622`.
It starts from upstream main `978775b5`, including merged filter-transform
PR [#1155](https://github.com/linebender/resvg/pull/1155), then applies
[#1163](https://github.com/linebender/resvg/pull/1163),
[#1164](https://github.com/linebender/resvg/pull/1164) and
[#1166](https://github.com/linebender/resvg/pull/1166). These retain the marker
viewport and shared local `feImage` paint fixes and improve text-path positioning.
The superseded affine `feImage` patch #1165 is omitted. Experimental fractional
pattern and font-hinting patches are not included. All 20 release jobs pass,
and all downloaded archives match their release digests. The binding's 23 tests
pass with both source-built and precompiled binaries; the core's 1,818 tests pass.
Ordinary Mix builds download the binary; they do not compile Rust. This Git dependency must be
replaced by a published compatible package before publishing this library to Hex.

`HtmlToPdf.render/2` is the public document workflow. `SvgRasterizer.rasterize/4`
is the internal byte adapter for complete SVG input, width/height options, the
shared image budget and authorized byte fonts. Its fifth argument carries the
validated processing mode and approved byte resources. The adapter returns PNG
bytes or the shared `{:error, {reason, diagnostic}}` result.

Extraction retains the source and relevant host styles. Font and resource inputs
must come from the existing authorization and budget scope. The converter uses
those supplied bytes, with system font discovery disabled. Scratch files needed
by its documented font/resource APIs do not grant permission to load arbitrary
files or network resources.

The original converter's output determines supported painting. The library does
not add custom gradient calculators, glyph stroke programs, pattern samplers,
mask compositors or hidden text behind the PNG. SVG text therefore does not
become extractable PDF text merely because it was painted into the image. Normal
HTML text surrounding the image keeps its ordinary font and text behavior.
Internal SVG links are inert in the PNG. An ordinary HTML anchor around the SVG
keeps its normal PDF link annotation.

Inline SVG, SVG images, backgrounds and list artwork use the shared
`Limits.svg_raster_scale` density, currently two device pixels per CSS pixel.
Previously image/background SVGs used one while inline SVG used two. Pixel
storage and budget charges use the dense raster dimensions; physical layout
keeps the used image dimensions. The PNG enters ordinary image placement without
an extra SVG translation group.

For PNGs with transparency, the PDF writer extends adjacent visible colors by one
pixel into fully transparent pixels. PDF readers interpolate the color image and
soft mask separately; leaving unrelated hidden colors produces dark or colored
edge fringes. Only zero-alpha color samples change. Visible samples, alpha,
placement and the lossless decoder result remain unchanged. This applies to
ordinary PNGs as well as converted SVG, including grayscale and 16-bit samples.

Selected object/embed/iframe SVG documents keep their child viewport. A root
transform is moved to an equivalent parent group when nesting that root in the
conversion canvas. Stock Resvg otherwise applies the nested root transform twice.
The retained viewport fixture verifies the correction against Chrome, and the
unit test checks pixels between the correct and doubled translation boundaries.

Internal tests for the deleted renderer's CID/Type3 resources, issued vector
Forms, native glyph observations and HTML effect slots are retired. Functional
source tests exercise real PNG pixels and PDF image embedding. Routine browser
acceptance covers extracting SVG from the document, converting it to PNG and
preserving its appearance, dimensions and placement. Representative gradients,
masks, clipping, text, fonts and authorized resources remain covered. Specialized
converter conformance probes are retired from routine acceptance; the original
fixtures and strict failure evidence are retained. The exact scope is recorded in
[the SVG browser audit](svg-browser-workflow-audit.md). Retained comparisons keep
their strict tolerances except the documented user-approved filter and
paint-server and gradient-alpha font-rendering allowances. SVG foreignObject HTML returns an explicit `unsupported_html` diagnostic from
`SvgConverterValidator` at the layout stage. The whole input is rejected because
the converter cannot paint that HTML; its descendants are never silently omitted.
Non-scaling strokes and SVG2 inline `textPath` path data also return explicit
converter diagnostics. Existing rendering expectations for those features must
assert the diagnostic while preserving the original Chromium reference. A
limitation case is not a successful pixel comparison. Other painting differences
remain failures under their configured browser thresholds until their causes are
verified.

For the user's fixed-size PNG workflow, eligible SVG comparisons also rasterize
the original SVG with Chrome at the same density, then replace it with that Chrome
PNG. Native extraction and conversion remain automatic. No Resvg output enters
the Chrome reference. All original SVGs are captured before replacement, SVG
stylesheets remain active, and the adapter rejects changes to surrounding CSS or
viewport geometry. It preserves transparency and avoids applying object-fit twice.

These comparisons use 192 DPI at the default density, normally retaining the
less-than-1% changed-pixel ceiling except the documented approved allowances.
The user-approved text paint-server, gradient-alpha and font-attributes fixtures
allow 2% changed pixels and retain the 1% mean-delta ceiling. Filter offsets
allows 10% changed pixels and 3% mean delta; filter text inputs allows 3% and 1%.
These fixture-specific exceptions retain the original strict failure evidence.
The 12-channel tolerance is unchanged. Combined samples keep independent
regions and visible-ink checks. Wrong color, size, position and missing-artwork
controls must fail. Each artifact directory also retains `chromium-vector.pdf`,
72-DPI strict rasters and `strict-vector-stats.exs`. That supplemental vector
comparison uses the PNG run's print viewport and device density; historical
strict runs from before this adapter remain separately preserved under
`tmp/browser-speed/`. A successful PNG comparison
does not turn a failed original vector comparison into a pass. SVG text extraction,
internal links and enlargement after PDF generation are outside this acceptance
scope; ordinary surrounding HTML text and links remain tested.

Further verified `resvg` deviations from Chromium painting remain in place.
Each was measured against both engines on the same input:

- resvg interpolates gradient stops in sRGB and ignores
  `color-interpolation="linearRGB"` on gradients and masks, including when the
  gradient feeds filter inputs, pattern tiles or mask content. Chromium
  linearizes channels first.
- resvg weights mask luminance from raw sRGB channels and ignores
  `color-interpolation` on `<mask>`; Chromium linearizes first.
- resvg clips the SourceGraphic filter input to the filter region before the
  primitive chain runs; Chromium does not, so `feOffset` and
  `feDisplacementMap` chains that move ink authored outside the region render
  empty under resvg.
- resvg orients automatic markers with the raw segment tangent: degenerate
  start tangents render horizontally, and closed subpath start and end markers
  keep separate incoming and outgoing tangents instead of Chromium's join
  bisector.
- resvg drops strokes of width at most one user unit on canvases of at most two
  device pixels.
- resvg renders percentage geometry (`cx`, `cy`, `r`) on marker children as
  nothing where identical numeric values paint.

Converter preparation re-encodes three deviations where source rewriting
reproduces Chromium exactly: zero-area marker viewBoxes, large glyph-turn angle
reduction and selected document root transforms. The marker viewport rewrite was
removed after qualifying the pinned converter fix; authored marker geometry now
reaches Resvg unchanged. The remaining deviations are
documented behavior of the established converter, not omitted features.

Converter preparation reserves the validated canvas dimensions and decoded image
planes against the shared limits. Repeated referenced images charge each
validated occurrence. The cumulative SVG surface reservation also includes a
conservative source-based estimate for opacity layers, nested viewports and
clipping. This estimate is not a measurement of Resvg's internal allocation and
does not reproduce the deleted renderer's temporary-surface work plan.
