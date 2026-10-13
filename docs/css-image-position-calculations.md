# Calculated image positions

Object and background positioning share the CSS math validator used by source
sizes. Percentages resolve against the difference between the actual image
viewport and the fitted image size. That difference can be negative for cover.
Edge offsets, nested numeric functions and percentage/length sums retain their
authored axis ordering until this space is known.

Font-relative values use the selected element font and used size. Root units
use the actual root font, size and line height carried by the style tree. The
source-size metric validator holds the single metric calculation body for
both initial source sizes and selected element positioning. Font payloads stay
opaque to the existing rem traversal. Viewport lengths use the document's
validated print viewport, including the embedded document's media viewport.

CSS escaped keywords, units and function names follow the shared token policy.
Invalid dimensions are discarded during the cascade. The layout executor
calls the position validator for a finite result and retains the existing
fitted image viewport clip. Image map regions continue to use that same final
selected image geometry.

The original `html_position_math.html` fixture compares object and background
positions against Chromium with the existing 1% thresholds. Focused tests also
cover cover fitting, zero font sizes, distinct root/element fonts and shared
failure diagnostics. Syntax follows [CSS Values positions](https://drafts.csswg.org/css-values-4/#position)
and [numeric functions](https://drafts.csswg.org/css-values-4/#math).
