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

SVGs appear as PNG artwork in the PDF. SVG text is rasterized and links inside
an SVG are inactive. Wrap the image in an HTML anchor to make it clickable.

Provide images and fonts through the supported [asset sources](html-to-pdf-compatibility.md#assets-and-local-files).
The converter cannot read arbitrary files or fetch network resources.
DTD/entity declarations and escaped CSS resource syntax are rejected.

`foreignObject`, non-scaling strokes and SVG2 inline `textPath` path data return
a diagnostic. Use a same-document path reference for supported text paths.

SVG conversion uses Resvg and can differ from browser rendering, particularly
for gradients, filters, masks, markers and text paths. Check complex artwork
against the output you need. See [diagnostics](diagnostics.md) for conversion
failures and [HTML and CSS support](html-to-pdf-compatibility.md#images-and-backgrounds)
for image sizing and styling.
