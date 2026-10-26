# Images

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

## JPEG images

The renderer embeds supported 8-bit baseline and progressive JPEGs. Grayscale,
RGB, YCbCr, CMYK, Adobe CMYK and Adobe YCCK retain their supported color conventions.
Malformed segments, inconsistent color metadata, empty scans and truncated streams
return diagnostics. Structural validation does not fully decode JPEG pixels;
PDF viewers decode the embedded image.

## SVG images

SVGs appear as PNG artwork in the PDF through the published `resvg` Hex dependency.
Elixir prepares source, dimensions, permitted font bytes and selected nested image
resources. The converter cannot read arbitrary caller paths or fetch network
resources. DTD/entity declarations and escaped CSS resource syntax are rejected.

The library diagnoses known unsupported converter features before returning an
incomplete image. These include `foreignObject`, non-scaling strokes and SVG2
inline `textPath` path data. SVG text is rasterized and internal SVG links are
inert. An ordinary HTML anchor around the image remains clickable.

SVG conversion does not provide complete browser SVG support. See
[HTML and CSS support](html-to-pdf-compatibility.md) for the supported rendering
profile and [diagnostics](diagnostics.md) for handling conversion failures.

Known converter differences include linear-RGB gradient and mask interpolation,
filter inputs moved from outside their filter region, some automatic marker
orientations and clipping, fractional pattern spacing, text-path positioning,
transformed image viewports, filter-image paint servers and strokes on very small
canvases. These can affect visible output even when conversion succeeds. Some
fixes are already merged into Rust resvg and still await a published Elixir
wrapper release. Converter-dependent browser tests retain explicit, bounded
allowances while those updates are pending.
