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

SVGs appear as raster images in the PDF. Use self-contained SVGs with internal
`#id` references. Embedded images, external resources, DTD/entity declarations,
and escaped CSS resource syntax are rejected. Remove any `DOCTYPE` declaration
from exported SVGs before using them.

SVG rendering uses Resvg, which can ignore unsupported SVG features and still
return an image. The binding does not report those omissions as conversion errors,
so this library cannot return diagnostics for them. A successful conversion
therefore does not guarantee that every SVG feature appears in the PDF.

See [image sources and supported styling](html-to-pdf-compatibility.md#images-and-backgrounds)
and [diagnostics](diagnostics.md) for usage details.
