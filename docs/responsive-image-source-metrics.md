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
