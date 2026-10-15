# PNG and SVG images

HTML-to-PDF rendering supports the following PNG formats, including interlaced
images:

| Color type           | Bit depths     |
| -------------------- | -------------- |
| Greyscale            | 1, 2, 4, 8, 16 |
| RGB                  | 8, 16          |
| Indexed palette      | 1, 2, 4, 8     |
| Greyscale with alpha | 8, 16          |
| RGBA                 | 8, 16          |

PNG color and transparency retain their full precision in the PDF, including
16-bit samples. Animated PNGs render their default image only. Color-profile
conversion is unsupported.

## SVG images

SVGs appear as PNG artwork in the PDF through the existing Resvg dependency.
Elixir prepares source, dimensions, permitted font bytes and selected nested image
resources. The converter cannot read arbitrary caller paths or fetch network
resources. DTD/entity declarations and escaped CSS resource syntax are rejected.

The library diagnoses known unsupported converter features before returning an
incomplete image. These include `foreignObject`, non-scaling strokes and SVG2
inline `textPath` path data. SVG text is rasterized and internal SVG links are
inert. An ordinary HTML anchor around the image remains clickable.

Successful conversion alone does not prove complete SVG conformance. The current
development baseline still has unresolved Chromium comparisons. See
[SVG conversion](svg-converter-workflow.md), the
[current work plan](milestone-1.1.0-work-plan.md) and
[diagnostics](diagnostics.md).
