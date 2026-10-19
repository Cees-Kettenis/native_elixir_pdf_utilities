# Math token device metrics

`generate.py` samples every glyph in the bundled STIX Two Math font using
FreeType's light hinting, then rounds the hinted outline control box outwards to
whole CSS pixels. This is the Linux Skia measurement path used by Chromium's
MathML `ComputeInkMetrics` through `PlainTextPainter`. The renderer measures
original token characters before math-auto. Selected OpenType script forms
retain their existing design-metric contract because Chromium's MathML painter
does not select them through ssty.

The data covers CSS font sizes from 1px to 128px in FreeType's 1/64px steps.
This range describes the generated dataset, not an admission or resource limit.
Other font contents and sizes retain the renderer's design metrics. The full
SHA-256 of the selected font gates use of this data, including configured copies
of the bundled font. Renaming a family does not change its metrics.

All 6,760 glyphs are sampled, independent of any HTML fixture, equation, token
content or font size used by a test. Runs of identical vertical bounds are stored
as transitions, then compressed. The Elixir reader embeds the profiles at compile
time and uses binary search at render time. Rendering needs no FreeType library,
C compiler or browser. Glyph advances, selected script/math-auto glyphs, PDF
outlines and logical text are unaffected.

To regenerate, install a C compiler, Python 3, pkg-config and FreeType development
headers, then run from the repository root:

```bash
python3 scripts/data/math-token-metrics/generate.py
```

The `.json` beside the binary records the input font hash, FreeType version,
load flags, sampling coverage and output digest. A FreeType upgrade may change
hinting. Review those changes against the original Chromium comparisons before
accepting a regenerated file. The current data uses FreeType 2.15.0.

The binary is derived from `priv/fonts/stix/STIXTwoMath-Regular.ttf`, distributed
under the SIL Open Font License 1.1 in that directory. The font bytes remain
unchanged. Its release and provenance are recorded in `provenance.json`.

References:

- [Blink token layout](https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/mathml/math_token_layout_algorithm.cc)
- [Skia FreeType metrics](https://skia.googlesource.com/skia/+/main/src/ports/SkFontHost_FreeType.cpp)
- [FreeType control boxes](https://freetype.org/freetype2/docs/reference/ft2-outline_processing.html#ft_outline_get_cbox)
