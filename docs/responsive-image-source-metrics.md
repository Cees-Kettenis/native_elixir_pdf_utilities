# Responsive image source-size metrics

Font-relative source sizes use the native document's initial selected default
font at 16 CSS pixels. Authored image font declarations do not change this
initial environment. `ex` and `cap` use real glyph bounds, `ch` uses the zero
glyph advance, `ic` uses the water ideograph advance, and `lh` uses the shared
normal line-height calculation. Root variants use the same initial environment.
CSS-defined missing-glyph fallbacks apply when the selected font lacks an
x-height, capital H or water ideograph.

The renderer measures fonts through the existing font validator and measurement
helpers. It resolves these metrics only for source-size or media values that
need them. It loads the selected image candidate through the existing asset
resolver; preparing font metrics does not request any unselected image URL.

Media conditions support nested parentheses, negation, conjunction and
disjunction, media types, and the existing dimension/resolution/aspect-ratio
features. Mixed unparenthesized `and` and `or`, missing operands and unbalanced
parentheses do not match. Explicit parentheses determine grouping. The media
parser and source-size arithmetic use the shared media-query depth and numeric
limits.

These rules follow the HTML [sizes parsing algorithm](https://html.spec.whatwg.org/multipage/images.html#parsing-a-sizes-attribute)
and [Media Queries font-relative unit rules](https://drafts.csswg.org/mediaqueries-5/#units).
The original `html_responsive_source_metrics.html` fixture compares resource
choices against Chromium without changing authored markup or thresholds.

Source-size and media calculations share `CssMathValidator`. It retains CSS
numeric types through multiplication and division, so expressions such as
`calc(8s / 2s * 16px)` produce lengths while `calc(1px / 1s)` does not.
The numeric functions include rounding, remainders, absolute value, sign,
hypotenuse, powers, roots, logarithms, exponentials and trigonometry, alongside
`calc`, `min`, `max` and `clamp`. CSS escapes work in function names, units,
media types, feature names and Boolean operators.

Unknown features, unknown values and general-enclosed conditions retain their
unknown result through negation and Boolean operations. They become false only
at the query boundary, following the [Media Queries three-valued logic](https://drafts.csswg.org/mediaqueries-4/#evaluating).
Unknown media types still evaluate to false. The original
`html_responsive_math.html` compares numeric image choices and these conditions
with Chromium. Its first failed comparison is retained in
`tmp/browser_parity/html_responsive_math-original-failure`.

Invalid dimensions are skipped by sizes parsing. NaN lengths resolve to zero.
An unbounded infinite length or a finite result exceeding the existing numeric
budget returns the shared resource-limit diagnostic. The implementation uses
native floating-point arithmetic within the library's finite resource limits.
See [CSS Values numeric functions](https://drafts.csswg.org/css-values-4/#math).
