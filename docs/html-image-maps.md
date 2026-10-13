# Static image maps

Map association follows the pinned HTML Standard [image-map processing model](https://html.spec.whatwg.org/multipage/image-maps.html#processing-model-2).
The first map with the exact hash-name in the same authored document/tree scope
is selected. Template contents are inert. A map retains its ordinary transparent
content; area elements have no paint box of their own.

ImageMapValidator owns association, permissive HTML coordinate parsing, shape
normalization and ordered hit testing. Rectangle endpoints are sorted, surplus
coordinates are discarded, polygon odd tails are removed, invalid/missing shape
keywords use Rectangle, and nonpositive circles and underspecified shapes are
empty. Circle, rectangle, even-odd polygon and default areas use CSS pixels after
image stretching; coordinates are not multiplied by the bitmap's pixel ratio.
The first covering area wins, including deliberate no-href masks. Destinations
use HtmlLinkValidator and the existing scoped URL/fragment target authority.

The final selected image's actual fit viewport and painted/clipped geometry
supply the annotation boundaries. ImageMapGeometry sweeps polygon edges into
nonoverlapping quadrilaterals, including concave/self-intersecting polygons,
overlap priority and deliberate holes. Circles are approximated only for PDF
quadrilateral serialization, with sagitta bounded by native layout's fixed 1/64
CSS-pixel precision; semantic hit testing retains the exact circle equation.

Nonpainting link-region boxes carry PDF QuadPoints, not an image or invisible
text substitute. Existing drawing-group composition transforms and clips their
coordinates; pagination translates them alongside the source image. The writer
clips them to the real page surface. `ImageMapValidator.lookup/4` uses the same
writer-prepared page geometry for transformed/clipped PDF-point queries, while
`hit_test/5` queries exact CSS-coordinate semantics. Ordinary block/atomic overflow clips use
their allocated padding viewport, while native controls retain their own client
and scrollbar handling.

Coordinate counts and arrangement work are bounded by shared Limits
`max_image_map_coordinates` and `max_image_map_geometry_work`; output boxes use
`max_layout_boxes`. Resource failures preserve the shared actionable diagnostics.
The original html_image_maps fixture exercises stretched/fitted images,
transparent map contents, source lookup/order, overflow clipping and fragment
links without changing native/Chromium inputs or comparison thresholds.
