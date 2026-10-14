# SVG converter integration checklist

SVG artwork is extracted from the HTML source, passed to the existing `Resvg`
converter, decoded as PNG, and embedded through the ordinary image writer. The
custom SVG painting engine, vector Forms, CID/Type3 glyph painting, glyph-source
observations and native HTML effect slots have been removed.

Host CSS preparation, layout dimensions and resource policy remain library
responsibilities. Shape, text, gradient, pattern, marker, mask and filter painting
belongs to the converter. The library must not introduce another painter or
patch a converter dependency to reproduce the removed engine.

| Contract | Required evidence |
| --- | --- |
| Extraction and host CSS | Retain original authored content, namespace identity, relevant inherited style and selected fonts. Compare original inline fixtures against Chromium. |
| Viewports and placement | Preserve image dimensions, `viewBox`, surrounding flow, inline baselines, embedded document sizing and page count. |
| Converter output | Exercise the real converter and decode its actual PNG bytes. Verify colored pixels, transparency and embedded image resources. |
| Resource policy | Reject active content, DTDs, external entities and unauthorized references before conversion. Approved resources use their real bytes. |
| Fonts | Supply authorized font bytes to the existing converter font database. Disable system font discovery. Do not substitute a face or add hidden PDF text. |
| Limits and diagnostics | Use shared limits for source, dependencies, raster dimensions, output and cumulative image work. Return the shared diagnostic contract. |
| Browser parity | Keep every original fixture, font, Chromium setting and strict comparison threshold. Converter omissions remain failures, not accepted transparent artwork. |
| Unsupported behavior | Report actual foreignObject, link-annotation and font limitations. A PNG image does not carry the removed vector text or interactive HTML field contracts. |

The original SVG browser fixtures remain the visible acceptance tests. Replacing
renderer-specific unit assertions does not qualify those fixtures. Publication
still requires the centrally owned quality matrix, all tests and meaningful
100% code coverage. Performance is the only permitted exception.
