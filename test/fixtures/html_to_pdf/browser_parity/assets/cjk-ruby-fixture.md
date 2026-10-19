# CJK ruby fixture font

This test-only TrueType font contains actual Noto Sans JP glyph outlines and
horizontal metrics at weight 400. `cjk-ruby-fixture.json` records the upstream
revision and source SHA-256, the subset SHA-256, and each retained glyph's outline
hash and metrics. The builder checks that subsetting leaves every outline and
horizontal metric identical to the instantiated source. No placeholder glyphs
or synthetic measurements are used.

Upstream source: https://github.com/google/fonts/tree/main/ofl/notosansjp
License: SIL Open Font License 1.1, retained in `cjk-ruby-fixture-LICENSE.txt`.
The subset uses the distinct family name `CJK Ruby Fixture`.

Rebuild using fonttools 4.66.1 and the exact source revision from the JSON manifest:

```
python scripts/fixtures/build-cjk-ruby-font-fixture.py NotoSansJP\[wght\].ttf --revision REVISION
```

The font is supplied explicitly to native and Chromium test renderers. Production
rendering does not discover host CJK fonts or register this fixture font.
