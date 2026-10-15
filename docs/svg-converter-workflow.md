# SVG conversion

The HTML renderer extracts SVG source and uses the existing `Resvg` dependency
to produce PNG artwork. That PNG follows the same decoding, layout and PDF image
embedding path as other images. SVG painting is not implemented by this library.

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

Internal tests for the deleted renderer's CID/Type3 resources, issued vector
Forms, native glyph observations and HTML effect slots are retired. Functional
source tests exercise real PNG pixels and PDF image embedding. Original browser
fixtures and strict tolerances remain unchanged. SVG foreignObject HTML returns an explicit `unsupported_html` diagnostic from
`SvgConverterValidator` at the layout stage. The whole input is rejected because
the converter cannot paint that HTML; its descendants are never silently omitted.
Non-scaling strokes and SVG2 inline `textPath` path data also return explicit
converter diagnostics. Existing rendering expectations for those features must
assert the diagnostic while preserving the original Chromium reference. A
limitation case is not a successful pixel comparison. Other painting differences
remain failures under the unchanged browser thresholds until their causes are
verified.

Converter preparation reserves the validated canvas dimensions and decoded image
planes against the shared limits. Repeated referenced images charge each
validated occurrence. The cumulative SVG surface reservation also includes a
conservative source-based estimate for opacity layers, nested viewports and
clipping. This estimate is not a measurement of Resvg's internal allocation and
does not reproduce the deleted renderer's temporary-surface work plan.
