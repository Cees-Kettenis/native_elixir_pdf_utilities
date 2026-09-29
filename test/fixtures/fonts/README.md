`wrapped-glyph.ttf` is derived from the bundled DejaVu Sans under
[its existing license](../../../priv/fonts/dejavu/LICENSE.txt).
Run `python scripts/build-wrapped-font-fixture.py` from the repository to rebuild it.

Its format-4 CMap maps A to glyph 40,000 through signed delta -25,601.
An unhinted rectangle at that glyph makes a missing mapping visually distinct
while avoiding differences between font rasterizers. Its advance uses the
original A metrics. Chromium and the native renderer compare this output.
