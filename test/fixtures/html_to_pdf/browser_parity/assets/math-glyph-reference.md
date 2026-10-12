The browser-only reference font is a renamed subset of pinned STIX Two Math
v2.13b171. It adds six PUA aliases for script and scriptscript forms because the
tested Chromium did not apply `ssty` on text or MathML.

Reproduce it from the repository root with Python and fontTools 4.60.1:

```sh
python scripts/generate-math-glyph-reference.py
```

The generator checks contours, per-glyph instructions, advances, and global hint
tables against the unchanged source font. Both Chromium and the native writer use
identical reference bytes. Native text maps selected forms to their original
Unicode. This fixture tests PDF glyph painting; native MathML layout is unfinished.

Source SHA-256: `49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b`.
Reference SHA-256: `efd3a29fd3c38b0540aa6518d7e4f1706306de288e8b2cb3e08a9695a1399582`.
The complete OFL is in `math-glyph-reference-LICENSE.txt`.

The same generator produces `math-row-reference.ttf`, an 8,032-byte subset
renamed to Math Row Test Reference. It retains selected MATH italic and accent
records and checks their values and device tables against STIX. Its SHA-256 is
`b0fd581692ec6dd843d8d9e7f4d5c2d89ca60589da1f13876ecc8e0c8306945e`.
The same complete OFL applies to this derivative.

`math_glyph_rows.html` uses equivalent CSS positions independently calculated
with fontTools from selected STIX advances, design-unit ink bounds, and italic
corrections. Native measurement uses unmodified STIX; both painting paths use
identical reference font bytes. The fixture tests 30-point glyph rows and
retains original Unicode. It does not assert native HTML/MathML composition.

The generator also preserves a direct MathML experiment at
`.quality/math-glyph-rows-unadjusted.html`. At 30 points Chromium's raster ink
bounds shifted the baseline by 0.54, 0.18, and 0.75 points for the three rows.
That comparison changed 3.640% of pixels. An equivalent CSS experiment at
30 points initially changed 1.020% near raster boundaries. The writer's
horizontal tie correction now makes that CSS reference pixel-identical.
The accepted row fixture has returned to 30 points. The direct MathML
ink-metric difference remains open for renderer acceptance.

The generator also produces `math-stretch-reference.ttf`, a 6,344-byte subset
renamed to Math Stretch Test Reference. Its SHA-256 is
`93aea0271109296f821b84aff9a07377bdbe190585b5d2dc73beb0f50f96b95d`.
The same complete OFL applies. `math_stretch_glyphs.html` uses exact rational
OpenType assembly calculations and compares natural glyphs, larger variants,
vertical assemblies and a horizontal brace at 30.75 points. Every retained
outline, instruction, advance and global hint matches the unchanged source.
The native writer retains one original operator through ActualText. Matching
264-point page dimensions avoid Chromium's print-page rounding of 260 points
to 259.92 points. Selected glyph painting resolves horizontal raster ties
toward the left using the writer's existing fixed precision bias.
